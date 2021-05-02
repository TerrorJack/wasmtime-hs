self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, libiconv, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage rec {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "92e0b6b9e8231bedfac0687da84b9d5683b29339";
          sha256 = "06ssq63z2bl5pf4485src4g13bayr72c0viv42kxz1l1mh5dlj1x";
          fetchSubmodules = true;
        };
        cargoHash = "sha256-5yyRujm44a1mK9z0AtxXRGw/MnLlj6/9q/cmtEZCzdI=";
        buildInputs = lib.optionals stdenv.isDarwin [
          libiconv
          darwin.apple_sdk.frameworks.Security
        ];
        cargoBuildFlags = [ "--package" "wasmtime-c-api" "--all-features" ];
        cargoTestFlags = cargoBuildFlags;
        postInstall = ''
          pushd crates/c-api
          mkdir $out/include
          cp \
            include/wasi.h \
            include/wasmtime.h \
            wasm-c-api/include/wasm.h \
            $out/include
          popd
        '';
      })
    { };
}
