let pkgs = import ./pkgs.nix; in { inherit (pkgs.haskellPackages) brechtserckx-be-hakyll; }

