let sources = import ./nix/sources.nix;
    pkgs = import ./nix/pkgs.nix;
in (import ./default.nix).shellFor {

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  withHoogle = true;

  tools = {
    cabal = "latest";
    hlint = "3.2.8";
    haskell-language-server = "1.8.0.0";
    ghcid = "latest";
    brittany = "0.13.1.2";
  };

  # You might want some extra tools in the shell (optional).
  buildInputs = with pkgs; [
    # hakyll-init
    haskellPackages.hakyll
    # web formatter
    nodePackages.js-beautify
    # niv
    (import sources.niv {}).niv
    # act
    (import sources.nixpkgs-act {}).act
  ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
