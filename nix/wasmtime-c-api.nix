self: _: {
  wasmtime = self.callPackage
    ({ darwin
     , fetchFromGitHub
     , lib
     , libiconv
     , rustPlatform
     , stdenv
     }: rustPlatform.buildRustPackage {
      pname = "wasmtime-c-api";
      version = "0.19.0";
      src = fetchFromGitHub {
        owner = "bytecodealliance";
        repo = "wasmtime";
        rev = "v0.26.0";
        sha256 = "19s1r2nsb1kak79djwk7ip5fqb4fy3v8wxirl71sdkzx1mc0cn37";
        fetchSubmodules = true;
      };
      cargoSha256 = "1mg7863f33vkwh4v8kncg6xpa1x5lv30g54ax12flzj2mq0kkjzl";
      cargoPatches = [
        ./wasmtime-c-api.patch
      ];
      buildInputs = lib.optionals stdenv.isDarwin [
        libiconv
        darwin.apple_sdk.frameworks.Security
      ];
      buildAndTestSubdir = "crates/c-api";
      postInstall = ''
        pushd crates/c-api
        cp -r include $out
        cp wasm-c-api/include/* $out/include
        popd
      '';
    })
    { };
}
