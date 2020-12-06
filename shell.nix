let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
  haskellDeps = ps: with ps; [ base vector unordered-containers parsec ];
  ghc = nixpkgs.haskellPackages.ghcWithPackages haskellDeps;
in with nixpkgs;
stdenv.mkDerivation {
  name = "hadvent-hof-hode";
  buildInputs = [
    ghc
    niv.niv
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.ormolu
  ];

}
