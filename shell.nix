let pkgs = import ./pkgs.nix;
    sources = import ./nix/sources.nix;
in pkgs.haskellPackages.shellFor {
  packages = p: [ p.brechtserckx-be ];
  buildInputs = with pkgs; [
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.brittany
    haskellPackages.hakyll
    nodePackages.js-beautify
    (import sources.niv {}).niv
    (import sources.nixpkgs-act {}).act
  ];
}
