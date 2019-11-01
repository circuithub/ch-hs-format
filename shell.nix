{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;
nixpkgs.mkShell {
  name = "ch-hs-format-shell";

  buildInputs = [
    (
      haskellPackages.ghcWithHoogle (
        hs: with hs; [
          base
          containers
          ghc
          ghc-exactprint
          optparse-applicative
        ]
      )
    )

    haskellPackages.cabal-install
  ];

  shellHook = ''
    NIX_GHC=$(type -p ghc)
    if [ -n "$NIX_GHC" ]; then
      eval $(grep export "$NIX_GHC")
    fi
  '';
}
