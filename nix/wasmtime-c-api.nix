self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "040d48128c415f30f9ce356637896a1ffbb7f915";
          sha256 = "sha256-mUf5zWaXtjemRsU5TfuOLT0daQ7dk5qTYYHRhkWX164=";
          fetchSubmodules = true;
        };
        cargoHash =
          "sha512-GArpTUeno82IFVtu0S1Ykc6/LaOoDYnflKXWRjkUJg4jsaLEcjPj1zjxeRgYM8TnfJ4AVTM+dGpp3+EhtM0O0w==";
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
