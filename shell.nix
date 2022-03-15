let pkgs = import ./pkgs.nix;
in pkgs.haskellPackages.shellFor {
  packages = p: [ p.brechtserckx-be ];
  buildInputs = with pkgs; [
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.brittany
    haskellPackages.hakyll
    nodePackages.js-beautify
  ];
}
