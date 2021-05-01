{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs
    (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays
        ++ [ (import ./nix/wasmtime-c-api.nix) ];
    })
, ghc ? "ghc8104"
, toolsGhc ? "ghc8104"
, hsPkgs ? import ./default.nix { inherit pkgs ghc; }
}:
hsPkgs.shellFor {
  packages = ps: with ps; [ wasmtime-hs wasmtime-hs-bindgen ];

  withHoogle = true;

  nativeBuildInputs = pkgs.lib.attrValues
    (pkgs.haskell-nix.tools toolsGhc {
      brittany = "latest";
      cabal = "latest";
      cabal-fmt = "latest";
      friendly = "latest";
      hindent = "latest";
      hlint = "latest";
      ormolu = "latest";
      stylish-haskell = "latest";
    }) ++ [
    (pkgs.haskell-nix.cabalProject {
      src = pkgs.fetchFromGitHub {
        owner = "haskell";
        repo = "haskell-language-server";
        rev = "1.1.0";
        sha256 = "0kviq3kinm3i0qm4r26rdnlkwbs1s3r1rqiqdry517rgkgnjpcp5";
        fetchSubmodules = true;
      };
      compiler-nix-name = ghc;
      configureArgs = "--disable-benchmarks --disable-tests";
    }).haskell-language-server.components.exes.haskell-language-server
    pkgs.clang-tools
    (import sources.niv { }).niv
    pkgs.nixfmt
    pkgs.nixpkgs-fmt
  ];

  exactDeps = true;

  WASMTIME_SRC = pkgs.wasmtime.src;
}
