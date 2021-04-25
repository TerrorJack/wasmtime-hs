import Language.C
import Language.C.System.GCC
import System.Environment.Blank
import System.FilePath

main :: IO ()
main = do
  [wasmtime_src] <- getArgs
  Right ast <-
    parseCFile
      (newGCC "gcc")
      Nothing
      ( ("-I" <>)
          <$> [ wasmtime_src </> "crates" </> "c-api" </> "include",
                wasmtime_src </> "crates" </> "c-api" </> "wasm-c-api" </> "include"
              ]
      )
      (wasmtime_src </> "crates" </> "c-api" </> "include" </> "wasmtime.h")
  print ast
