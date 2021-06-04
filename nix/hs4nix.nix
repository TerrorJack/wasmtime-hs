{ sources ? import ./sources.nix { }
, pkgs ? import sources.nixpkgs { overlays = [ (import ./wasmtime-c-api.nix) ]; }
}:
with pkgs;
mkShell {
  nativeBuildInputs = [
    cabal-install
    (haskellPackages.ghcWithPackages (ps: with ps; [ unliftio vector ]))
    (import
      (fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "5730083206024c17d41fa33a88538bc84246284b";
        sha256 = "sha256-fxzQ6w1pe+4xEjH2f/HrnUt9Pd/fDBeNmxAAtc2npmM=";
      })
      { }).netlify-cli
  ];
  buildInputs = [ wasmtime ];
}
