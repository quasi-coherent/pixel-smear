{ compiler ? "ghc865"
, nixpkgs ? import (import ./nix/sources.nix).nixpkgs {}
}:

let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  hsPkgs = nixpkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "pixel-smear" =
        self.callCabal2nix
          "pixel-smear"
          (gitignore ./.)
          {};
    };
  };

  shell = hsPkgs.shellFor {
    packages = ps: [
      ps."pixel-smear"
    ];

    buildInputs = with nixpkgs.haskellPackages; [
      ghcid
      hlint
      hpack
      hsPkgs.cabal-install
      stylish-haskell
    ];

    withHoogle = true;
  };

  exe = nixpkgs.haskell.lib.justStaticExecutables (hsPkgs."pixel-smear");

in { inherit shell; inherit exe; }
