{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs { overlays = [ (import ./wasmtime-c-api.nix) ]; }
}:
with pkgs;
mkShell {
  nativeBuildInputs = [
    cabal-install
    (haskellPackages.ghcWithPackages (ps: with ps; [ unliftio vector ]))
  ];
  buildInputs = [ wasmtime ];
}
