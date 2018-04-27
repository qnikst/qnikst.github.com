{ pkgs ? import <nixpkgs> {} }:

let drv = pkgs.haskellPackages.callCabal2nix "qnikst-github-com" ./. {};
in drv.env
