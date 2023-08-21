{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/834a120d3607fbb5fd9151f6881c77f489e9e94b"; 
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs-act.url = "github:NixOS/nixpkgs/8540876319735c0891f25c8c7c9d8475159ae8fe";
  outputs = { self, nixpkgs, flake-utils, haskellNix, nixpkgs-act }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          hsPkgs =
            final.haskell-nix.project {
              src = final.haskell-nix.haskellLib.cleanGit {
                name = "brechtserckx-be";
                src = ./.;
              } ;

              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell = {
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

                # Non-Haskell shell tools go here
                buildInputs = with pkgs; [
                  # nix formatter
                  nixpkgs-fmt
                  # web formatter
                  nodePackages.js-beautify
                  # run github workflows locally
                  nixpkgs-act.legacyPackages.${system}.act
                ];

                # Prevents cabal from choosing alternate plans, so that
                # *all* dependencies are provided by Nix.
                exactDeps = true;
              };
            };
          brechtserckx-be-site = pkgs.stdenv.mkDerivation {
            name = "brechtserckx-be-site";
            version = "1.0";
            buildInputs = [final.hsPkgs.brechtserckx-be.components.exes.brechtserckx-be ];
            src = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ] ./.;
            phases = "unpackPhase buildPhase";
            buildPhase = ''
                export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
                export LANG=en_US.UTF-8
                brechtserckx-be build

                mkdir $out
                cp -r _site/* $out
            '';
          } ;

        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.hsPkgs.flake {};

    in flake // {
        # Built by `nix build .`
        packages.default = pkgs."brechtserckx-be-site";
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
