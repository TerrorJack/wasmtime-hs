self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage rec {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "60f7b23ea122440b5a544f1ecf4f6858d69b1f64";
          sha256 = "0lscxqrblq13p7m41gjnaq2mbyaf3acbxz3kaa0y1kj0l9yzycar";
          fetchSubmodules = true;
        };
        cargoHash = "sha256-8hN9rmE7cVQJvQXo5ZD9EnwbGB2l3gapz+DwEchpwdw=";
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
