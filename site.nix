let
  pkgs = import nix/pkgs.nix;
  stdenv = pkgs.stdenv;
  site = (import ./default.nix).brechtserckx-be.components.exes.brechtserckx-be;
in pkgs.stdenv.mkDerivation {
  name = "brechtserckx-be";
  version = "1.0";
  buildInputs = [site];
  src = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ] ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
      export LANG=en_US.UTF-8
      brechtserckx-be build

      mkdir $out
      cp -r _site/* $out
  '';
}
