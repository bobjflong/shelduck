# run.nix

with (import <nixpkgs> {});
haskellPackages.callPackage (import ./project.nix) {}
