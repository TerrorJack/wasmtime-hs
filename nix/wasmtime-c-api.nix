self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage rec {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "ec5d8016f7c657dee1c1cc1e2715783e74709461";
          sha256 = "0fxp1ci5pj310hy9cx961r8q0hwpy99la88gvn157wsmmxhxznmx";
          fetchSubmodules = true;
        };
        cargoHash = "sha256-L+/ZLwnvA8yoWlKVOfvzveBUETyLGG1q5L1owPnG5v4=";
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
