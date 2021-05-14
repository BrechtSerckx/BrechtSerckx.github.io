let
  sources = import ./nix/sources.nix;
  overlay = self: super:
    let
      hlib = super.haskell.lib;
      lib = super.lib;
      gitignore = path:
        super.nix-gitignore.gitignoreSourcePure [ (path + /.gitignore) ] path;
      overrides = selfh: superh: {
        brechtserckx-be-hakyll = superh.callCabal2nix "brechtserckx-be-hakyll" (gitignore ./.) {};
      };
    in {
      haskellPackages = super.haskellPackages.override (old: {
        overrides =
          lib.composeExtensions (old.overrides or (_: _: { })) overrides;
      });
    };
in import sources.nixpkgs { overlays = [overlay]; }
