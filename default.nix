# You'll want to ping those
import <nixpkgs> {
  config = {};
  overlays =
    [ (self: super:
       { haskellPackages =
           super.haskellPackages.extend
           (super.haskell.lib.packageSourceOverrides
              { qnikst-github-com = self.lib.cleanSource ./.; }
           );
        })
    ];
}
