let pkgs = import ./pkgs.nix;
in pkgs.haskellPackages.shellFor {
  packages = p: [ p.brechtserckx-be-hakyll ];
  buildInputs = with pkgs; [
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
  ];
}
