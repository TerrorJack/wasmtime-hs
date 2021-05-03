self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage rec {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "7a3791f9e9905387c747476334253732fa3e9199";
          sha256 = "0w0yv8bvlk47ky2grlk4xzhw3yyfvic0qsvshvgjmpv34wbki7jv";
          fetchSubmodules = true;
        };
        cargoHash = "sha256-5yyRujm44a1mK9z0AtxXRGw/MnLlj6/9q/cmtEZCzdI=";
        buildInputs = lib.optionals stdenv.isDarwin [
          darwin.libiconv
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
