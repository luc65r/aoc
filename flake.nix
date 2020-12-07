{
  description = "My solutions for Advent of Code";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux = with import nixpkgs {
      system = "x86_64-linux";
    }; stdenv.mkDerivation {
      name = "aoc";
      src = self;

      dontConfigure = true;
      dontInstall = true;
      doCheck = true;

      buildPhase = ''
        rm ./2020/01/report.nix # fails to patch shebang
        patchShebangs .
      '';

      checkPhase = ''
        ./test.sh
      '';

      nativeBuildInputs = [
        gcc
        ghc
        zig
        rustc
        rakudo
        rlwrap
      ];
    };
  };
}
