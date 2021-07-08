self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "c71ad9490e7f3e19bbcae7e28bbe50f8a0b4a5d8";
          sha256 = "sha256-odRrxqMQPmWO1c0HLjZKG4psdXt8ep7A0IAXjMh2VPA=";
          fetchSubmodules = true;
        };
        cargoHash =
          "sha512-Y8uskIDfVlRZsWefhmCOC9pQ2ilc8saeudWQc0Q9qqET+X12XadMhpeypgWV4sfyggfGRdkcyWeq1LwTZV1FfA==";
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
