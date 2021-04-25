{ sources ? import ./sources.nix { }
, pkgs ? import sources.nixpkgs { }
}: pkgs.fetchFromGitHub {
  owner = "bytecodealliance";
  repo = "wasmtime";
  rev = "dev";
  sha256 = "0g36kz0ic5kicp525yfdwi7pmgdhscrr3y8jpy2i1i4p1798nyfz";
  fetchSubmodules = true;
}
