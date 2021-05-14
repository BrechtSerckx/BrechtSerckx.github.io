let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  stdenv = pkgs.stdenv;
  site = import ./default.nix;
in pkgs.stdenv.mkDerivation {
  name = "brechtserckx-be";
  version = "1.0";
  buildInputs = [site];
  src = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ] ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
      export LANG=en_US.UTF-8
      brechtserckx-be-hakyll build

      mkdir $out
      cp -r _site/* $out
  '';
}
