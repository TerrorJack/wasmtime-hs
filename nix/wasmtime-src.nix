{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? haskellNix.pkgs-unstable
}: pkgs.fetchFromGitHub {
  owner = "bytecodealliance";
  repo = "wasmtime";
  rev = "v0.26.0";
  sha256 = "19s1r2nsb1kak79djwk7ip5fqb4fy3v8wxirl71sdkzx1mc0cn37";
  fetchSubmodules = true;
}
