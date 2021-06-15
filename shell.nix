{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs
    (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays
        ++ [ (import ./nix/wasmtime-c-api.nix) ];
    })
, ghc ? "ghc8105"
, toolsGhc ? "ghc8105"
, hsPkgs ? import ./default.nix { inherit pkgs ghc; }
}:
let
  ghc_ver = pkgs.haskell-nix.compiler."${ghc}".version;
  ghc_pre_9 = !(pkgs.lib.versionAtLeast ghc_ver "9");
in
hsPkgs.shellFor {
  packages = ps: with ps; [ wasmtime-hs wasmtime-hs-bindgen ];

  withHoogle = ghc_pre_9;

  tools =
    let
      args = {
        version = "latest";
        compiler-nix-name = toolsGhc;
      };
    in
    {
      brittany = args;
      cabal-fmt = args;
      floskell = args;
      friendly = args;
      ghcid = args;
      hlint = args;
      hoogle = args;
      ormolu = args;
      stylish-haskell = args;
    };

  nativeBuildInputs = pkgs.lib.optionals ghc_pre_9 [
    (pkgs.haskell-nix.cabalProject {
      src = pkgs.fetchFromGitHub {
        owner = "haskell";
        repo = "haskell-language-server";
        rev = "1.2.0";
        sha256 = "sha256-vNpb/Fjb9sKkt/1r29c82P/NeAXpxQbVKnM/EDWY6z4=";
        fetchSubmodules = true;
      };
      compiler-nix-name = ghc;
      configureArgs =
        "--disable-benchmarks --disable-tests -fall-formatters -fall-plugins";
    }).haskell-language-server.components.exes.haskell-language-server
  ] ++ [
    pkgs.haskell-nix.internal-cabal-install
    pkgs.clang-tools
    pkgs.niv
    pkgs.nixfmt
    pkgs.nixpkgs-fmt
    pkgs.wabt
  ];

  exactDeps = true;

  LD_LIBRARY_PATH = [ "${pkgs.wasmtime}/lib" ];

  WASMTIME_SRC = pkgs.wasmtime.src;

  RUST_BACKTRACE = "full";
}
