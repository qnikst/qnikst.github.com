with import <nixpkgs> {};

import <nixpkgs> {
  config = {};
  overlays = 
    [ (self: super:
       { haskellPackages =
           super.haskellPackages.extend
           (super.haskell.lib.packageSourceOverrides
              { qnikst-blog = self.lib.cleanSource ./.; }
           );
        })
    ];
}
