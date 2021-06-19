{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs
    (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays
        ++ [ (import ./nix/wasmtime-c-api.nix) ];
    })
, ghc ? "ghc8105"
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "wasmtime-hs";
    src = ./.;
  };
  compiler-nix-name = ghc;
  modules = [{ dontPatchELF = false; } { dontStrip = false; }];
}
