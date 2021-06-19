self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "7ce46043dc50884f982e05c770aa01ea5bca73f0";
          sha256 = "sha256-iepsXu+PVhEmKH5cTx/50yYNCqS/IiJzD6Z2fJFONjU=";
          fetchSubmodules = true;
        };
        cargoHash =
          "sha512-P/VNDDB/ZhDN7/Z3HYdOfuTwdkn1pH+rN30Yl+ULTN4NtmdOxIW3NxihXHZ5UK63l78X/kxjinqFAeOaJ3OB4g==";
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
