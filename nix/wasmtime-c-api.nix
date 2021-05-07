self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage rec {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "002b0744eb5a66c11751014e0fbcfa94a0289233";
          sha256 = "0sf2kvcqwbbwnybpn96prvj4ynzrcg9zmg2n521y7k2iv1yndp6k";
          fetchSubmodules = true;
        };
        cargoHash = "sha256-L+/ZLwnvA8yoWlKVOfvzveBUETyLGG1q5L1owPnG5v4=";
        buildInputs = lib.optionals stdenv.isDarwin
          (with darwin; [ libiconv apple_sdk.frameworks.Security ]);
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
