let pkgs = import ./nix/pkgs.nix;
in pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "brechtserckx-be";
    src = ./.;
  };
}

