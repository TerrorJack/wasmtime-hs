{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import qualified Data.HashSet as HS
import Data.List
import qualified Data.Map.Strict as M
import Distribution.Simple.Utils (unintersperse)
import Language.C
import Language.C.Analysis
import Language.C.Data.Ident
import Language.C.System.GCC
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.PprLib as TH
import System.Environment.Blank
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint
  ( Mode (..),
    Style (..),
    render,
    renderStyle,
    style,
  )
import UnliftIO
import UnliftIO.Process

{-# NOINLINE wasmtimeSrc #-}
wasmtimeSrc :: FilePath
wasmtimeSrc = unsafePerformIO $ do
  Just p <- getEnv "WASMTIME_SRC"
  pure p

{-# NOINLINE wasmtimeSyms #-}
wasmtimeSyms :: HS.HashSet String
wasmtimeSyms = unsafePerformIO $ do
  Just p <- getEnv "LD_LIBRARY_PATH"
  r <- readProcess "nm" ["-gDP", p </> "libwasmtime.so"] ""
  evaluate $ HS.fromList [head $ words l | l <- lines r]

wasmtimeSafeFuncs :: HS.HashSet String
wasmtimeSafeFuncs =
  HS.fromList
    [ "wasm_func_call",
      "wasm_instance_new",
      "wasmtime_linker_instantiate",
      "wasmtime_func_call",
      "wasmtime_instance_new"
    ]

posIsWasm :: Pos a => a -> Bool
posIsWasm a =
  unsafePerformIO $
    catchAny
      (evaluate $ wasmtimeSrc `isPrefixOf` posFile (posOf a))
      (\_ -> pure False)

wasmGlobalDecls :: GlobalDecls -> GlobalDecls
wasmGlobalDecls GlobalDecls {..} =
  GlobalDecls
    { gObjs = M.filter posIsWasm gObjs,
      gTags = M.filter posIsWasm gTags,
      gTypeDefs = M.filter posIsWasm gTypeDefs
    }

renderTH :: TH.Ppr a => a -> String
renderTH = renderStyle style {mode = OneLineMode} . TH.to_HPJ_Doc . TH.ppr

(.->.) :: TH.Type -> TH.Type -> TH.Type
a .->. b = TH.AppT TH.ArrowT a `TH.AppT` b

(.~>.) :: [TH.Type] -> TH.Type -> TH.Type
arg_tys .~>. ret_ty = foldr (.->.) ret_ty arg_tys

mkTHType :: String -> TH.Type
mkTHType = TH.ConT . TH.mkName

fromSnakeCase :: String -> String
fromSnakeCase s = mconcat toks''
  where
    toks = unintersperse '_' s

    toks'
      | last toks == "t" = init toks
      | otherwise = toks

    toks'' = [toUpper c : cs | c : cs <- toks']

renderTypeIdent :: Ident -> Builder
renderTypeIdent (Ident s _ _) = stringUtf8 $ fromSnakeCase s

renderIdent :: Ident -> Builder
renderIdent (Ident s _ _) = stringUtf8 s

cvtIntType :: IntType -> TH.Type
cvtIntType TyBool = mkTHType "CBool"
cvtIntType TyChar = mkTHType "CChar"
cvtIntType TySChar = mkTHType "CSChar"
cvtIntType TyUChar = mkTHType "CUChar"
cvtIntType TyShort = mkTHType "CShort"
cvtIntType TyUShort = mkTHType "CUShort"
cvtIntType TyInt = mkTHType "CInt"
cvtIntType TyUInt = mkTHType "CUInt"
cvtIntType TyLong = mkTHType "CLong"
cvtIntType TyULong = mkTHType "CULong"
cvtIntType TyLLong = mkTHType "CLLong"
cvtIntType TyULLong = mkTHType "CULLong"
cvtIntType it = error $ show it

cvtFloatType :: FloatType -> TH.Type
cvtFloatType TyFloat = mkTHType "CFloat"
cvtFloatType TyDouble = mkTHType "CDouble"
cvtFloatType ft = error $ show ft

cvtTypeName :: TypeName -> TH.Type
cvtTypeName TyVoid = TH.TupleT 0
cvtTypeName (TyIntegral it) = cvtIntType it
cvtTypeName (TyFloating ft) = cvtFloatType ft
cvtTypeName (TyComp (CompTypeRef (NamedRef (Ident s _ _)) _ _)) =
  mkTHType $ fromSnakeCase s
cvtTypeName tn = error $ show tn

cvtSingleType :: Type -> TH.Type
cvtSingleType (DirectType ty_name _ _) = cvtTypeName ty_name
cvtSingleType (PtrType (FunctionType ft _) _ _) =
  mkTHType "FunPtr" `TH.AppT` cvtFunType ft
cvtSingleType (PtrType ty _ _)
  | ty' == mkTHType "CChar" = mkTHType "CString"
  | otherwise = mkTHType "Ptr" `TH.AppT` cvtSingleType ty
  where
    ty' = cvtSingleType ty
cvtSingleType (ArrayType ty _ _ _)
  | ty' == mkTHType "CChar" = mkTHType "CString"
  | otherwise = mkTHType "Ptr" `TH.AppT` cvtSingleType ty
  where
    ty' = cvtSingleType ty
cvtSingleType (TypeDefType (TypeDefRef (Ident s _ _) _ _) _ _) = case s of
  "size_t" -> mkTHType "CSize"
  "uint8_t" -> mkTHType "Word8"
  "uint32_t" -> mkTHType "Word32"
  "uint64_t" -> mkTHType "Word64"
  _ -> mkTHType $ fromSnakeCase s
cvtSingleType ty = error $ show ty

paramDeclType :: ParamDecl -> Type
paramDeclType (ParamDecl (VarDecl _ _ ty) _) = ty
paramDeclType (AbstractParamDecl (VarDecl _ _ ty) _) = ty

cvtFunType :: FunType -> TH.Type
cvtFunType (FunType ret_type params _) =
  [cvtSingleType $ paramDeclType param | param <- params]
    .~>. TH.AppT (mkTHType "IO") (cvtSingleType ret_type)
cvtFunType (FunTypeIncomplete ret_type) =
  mkTHType "IO" `TH.AppT` cvtSingleType ret_type

cvtDeclType :: Type -> TH.Type
cvtDeclType (FunctionType ft _) = cvtFunType ft
cvtDeclType ty = error $ show ty

renderIdentDecl :: Ident -> IdentDecl -> Builder
renderIdentDecl _ (Declaration (Decl (VarDecl (VarName i@(Ident s _ _) _) _ ty) _))
  | s `HS.member` wasmtimeSyms =
    "foreign import ccall "
      <> (if s `HS.member` wasmtimeSafeFuncs then "safe" else "unsafe")
      <> " \""
      <> renderIdent i
      <> "\" "
      <> renderIdent i
      <> " :: "
      <> stringUtf8 (renderTH (cvtDeclType ty))
      <> "\n"
  | otherwise =
    unsafePerformIO $ do
      putStrLn s
      pure mempty
renderIdentDecl _ _ = mempty

renderTypeDef :: Ident -> TypeDef -> Builder
renderTypeDef _ (TypeDef i ty _ _) =
  "type "
    <> renderTypeIdent i
    <> " = "
    <> stringUtf8 (renderTH (cvtSingleType ty))
    <> "\n"

positionSortKey :: Position -> (Int, String, Int)
positionSortKey pos =
  ( case takeFileName $ posFile pos of
      "wasm.h" -> 0
      "wasi.h" -> 1
      "wasmtime.h" -> 3
      _ -> 2,
    takeFileName $ posFile pos,
    posOffset pos
  )

reorder :: Pos v => M.Map k v -> [(k, v)]
reorder = sortOn (positionSortKey . posOf . snd) . M.toList

renderGlobalDecls :: GlobalDecls -> Builder
renderGlobalDecls GlobalDecls {..} =
  foldMap (uncurry renderTypeDef) (reorder gTypeDefs)
    <> foldMap (uncurry renderIdentDecl) (reorder gObjs)

main :: IO ()
main = do
  Right ast_raw <-
    parseCFile
      (newGCC "gcc")
      Nothing
      ( ("-I" <>)
          <$> [ wasmtimeSrc </> "crates" </> "c-api" </> "include",
                wasmtimeSrc </> "crates" </> "c-api" </> "wasm-c-api" </> "include"
              ]
      )
      (wasmtimeSrc </> "crates" </> "c-api" </> "include" </> "wasmtime.h")
  let Right (wasmGlobalDecls -> ast, []) = runTrav_ $ analyseAST ast_raw
  writeFile "ast.txt" $ render $ pretty ast
  LBS.writeFile "ast.hs.txt" $ toLazyByteString $ renderGlobalDecls ast
