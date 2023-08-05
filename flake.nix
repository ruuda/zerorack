{
  description = "A DSL for constructing zero-knowledge proofs.";

  inputs.nixpkgs.url = "nixpkgs/nixos-23.05";

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
          name = "zerorack";
          nativeBuildInputs = [
            ghc
            pkgs.gnumake
          ];
        };
      };
}
