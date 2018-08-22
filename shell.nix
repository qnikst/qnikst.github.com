let
  pkgs = import ./.;
  ROOT = builtins.toString ./.;
in pkgs.haskellPackages.shellFor {
  packages=p: [p.qnikst-github-com];
  shellHook=
    ''
      blog_build() {
        pushd ${ROOT}
        cabal new-build
        popd
      }
    '';
  buildInputs = [ pkgs.cabal-install ];
  }
