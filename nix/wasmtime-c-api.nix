self: _: {
  wasmtime = self.callPackage
    ({ darwin, fetchFromGitHub, lib, rustPlatform, stdenv }:
      rustPlatform.buildRustPackage {
        pname = "wasmtime-c-api";
        version = "0.19.0";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wasmtime";
          rev = "1770880e19a771d23715e2d1718cc8d08dbcff80";
          sha256 = "sha256-zqdql3OZmyLTU7GFUMa/8+MOTQtw8MWpO2owJtwAaPA=";
          fetchSubmodules = true;
        };
        cargoHash =
          "sha512-Cr+PX6lPmwO1uGrhfhaO8OhmrhX5dFX2QneVZ/gIP6hL9ptyOaEC0QvLer1JnwrNB4SRkuLl7DS5XWiwg8oGQw==";
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
