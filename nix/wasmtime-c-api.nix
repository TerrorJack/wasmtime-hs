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
        rev = "dev";
        sha256 = "0g36kz0ic5kicp525yfdwi7pmgdhscrr3y8jpy2i1i4p1798nyfz";
        fetchSubmodules = true;
      };
      cargoSha256 = "1ydn2d8y9rcgq3xq9gwy3mj1krk89lf90g5shqwjwqlc26ys6m1x";
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
