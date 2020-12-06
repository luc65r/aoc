{
  description = "My solutions for Advent of Code";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux = with import nixpkgs {
      system = "x86_64-linux";
    }; stdenv.mkDerivation {
      name = "aoc";
      src = self;
      buildPhase = "";
      installPhase = "";

      buildInputs = [
        gcc
        ghc
        zig
        rakudo
        rlwrap
      ];
    };
  };
}
