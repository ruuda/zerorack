{
  description = "A DSL for constructing zero-knowledge proofs.";

  inputs.nixpkgs.url = "nixpkgs/432fc2d9a67f92e05438dff5fdc2b39d33f77997";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      ghc = pkgs.haskellPackages.ghcWithPackages (p: [
        p.text
        p.unordered-containers
      ]);
    in
      {
        devShells.x86_64-linux.default = pkgs.mkShell {
          nativeBuildInputs = [
            ghc
          ];
        };
      };
}
