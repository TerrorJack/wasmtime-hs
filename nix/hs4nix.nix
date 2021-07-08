{ sources ? import ./sources.nix { }
, pkgs ? import sources.nixpkgs { overlays = [ (import ./wasmtime-c-api.nix) ]; }
, ghc ? "ghc8104"
}:
with pkgs;
mkShell {
  nativeBuildInputs = [
    cabal-install
    (haskell.packages."${ghc}".ghcWithHoogle
      (ps: with ps; [ unliftio unordered-containers vector ]))
  ];
  buildInputs = [ wasmtime ];
  LD_LIBRARY_PATH = [ "${pkgs.wasmtime}/lib" ];
  WASMTIME_SRC = pkgs.wasmtime.src;
}
