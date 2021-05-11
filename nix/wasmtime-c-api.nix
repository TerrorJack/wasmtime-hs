self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage rec {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "db8ccb54b67016a650732e4a6c38fb4137adfdb9";
          sha256 = "0g5pqszqcnm1zjnwc598hhr2r3jch4zvn2adq279rx0bibx08pby";
          fetchSubmodules = true;
        };
        cargoHash = "sha256-9Fj42uAEIkU9rZKCgAUFV94KvUyNbGniKw2Y+a3NjY4=";
        buildInputs = lib.optionals stdenv.isDarwin
          (with darwin; [ libiconv apple_sdk.frameworks.Security ]);
        buildType = "debug";
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
