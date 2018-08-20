let
  pkgs = import ./.;
in pkgs.haskellPackages.shellFor {
  packages=p: [p.qnikst-blog];
  shellHook=
    ''
    alias hello=echo "hello"
    '';
  }
