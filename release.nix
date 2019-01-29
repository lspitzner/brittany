{ pkgs ? import (fetchGit (import ./pkgs.nix)) {}
, compiler ? "ghc822"
}:

pkgs.haskell.packages.${compiler}.callPackage ./shell.nix {}
