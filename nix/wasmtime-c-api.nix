self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "3d56728b8698d538a00d9f1c295149bbf5e8b78b";
          sha256 = "sha256-+Nn91yd074SE4PVggTHRmjpcy+i9BjGhUdGO8WvWoi4=";
          fetchSubmodules = true;
        };
        cargoHash =
          "sha512-13sSiIiL8UFO6rbh0xfrrAmU1KjgEgWfFuljxldhEH1Mg0VLOfRijhvI4Yl9CFUEcZhs4RRyVTf8ibWlp3/i3g==";
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
