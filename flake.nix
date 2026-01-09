{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
  inputs.nixpkgsMaster.url = "github:NixOS/nixpkgs/master";

  outputs = { self, flake-utils, gitignore, nixpkgs, nixpkgsMaster }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        compiler-nix-name = "ghc9122";

        pkgsMaster = import nixpkgsMaster { inherit system; };

        pkgs = import nixpkgsMaster { inherit system; };

      in
        {
          devShells = {
            default = pkgs.mkShell {
              NIX_PATH = "nixpkgs=${pkgsMaster.path}";
              buildInputs = with pkgs; [
                zlib

                haskell.compiler.ghc9122
              ];
            };
          };
        }
    );
}
