self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "57375588f2bc125f5f48fc56b4dedc8d5d7a2aeb";
          sha256 = "sha256-IRzwLPfbl1hs6+JylCzoawya3vQzRKLdqy28q99LLLM=";
          fetchSubmodules = true;
        };
        cargoHash =
          "sha512-VRTTTRk4ErbUpuv7L48qb3MYfWviKgIN9ND1hoXLIRbJdjb2Qb/NmD2bzJnvGcN8yI/nQZw9tpGqi9m+fG6IUA==";
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
