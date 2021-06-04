self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "357b4c7b60fa8e531848c4f6fb56f5a6a7b445e0";
          sha256 = "sha256-5Q9x1um7Bonm6bHNlPcmCtF7CMjK32kFTrG2eLS/8tE=";
          fetchSubmodules = true;
        };
        cargoHash =
          "sha512-pbdI1btBVavqL+V9yW6WiuYOae+X8UgMh2Zx7gck+bBMyfM5+eUxz2NnprTJkuEk9XknPo0fTVqO6Czc+jicjQ==";
        buildInputs = lib.optionals stdenv.isDarwin
          (with darwin; [ libiconv apple_sdk.frameworks.Security ]);
        buildType = "debug";
        cargoBuildFlags = [ "--package" "wasmtime-c-api" "--all-features" ];
        cargoTestFlags = [ "--package" "wasmtime" ];
        preCheck = ''
          export HOME="$TMP"
        '';
        postInstall = ''
          pushd crates/c-api
          cp -r include $out
          cp \
            wasm-c-api/include/* \
            $out/include
          popd
        '';
      })
    { };
}
