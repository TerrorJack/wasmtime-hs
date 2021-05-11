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
  packages = ps: with ps; [ wasmtime-hs ];

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
        rev = "ghcide-v1.3.0";
        sha256 = "07b8xjjsd5g4lh9c1klak7gnlss5zwb6dad2cgdxry9jhx7w4z7m";
        fetchSubmodules = true;
      };
      compiler-nix-name = ghc;
      configureArgs = "--disable-benchmarks --disable-tests";
    }).haskell-language-server.components.exes.haskell-language-server
    pkgs.clang-tools
    (import sources.niv { }).niv
    pkgs.nixfmt
    pkgs.nixpkgs-fmt
    pkgs.wabt
  ];

  exactDeps = true;

  LD_LIBRARY_PATH = [ "${pkgs.wasmtime}/lib" ];

  RUST_BACKTRACE = "full";
}
