let
  pkgs = import ./.;
  ROOT = builtins.toString ./.;
in pkgs.haskellPackages.shellFor {
  packages=p: [p.qnikst-github-com];
  shellHook=
    ''
      alias hello=echo "hello"

      foo_build() {
        pushd ${ROOT}
        cabal new-build
        popd
      }
    '';
  buildInputs = [ pkgs.cabal-install ];
  }
