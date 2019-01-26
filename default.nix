{ pkgs ? import (fetchGit (import ./pkgs.nix)) {}
, compiler ? "ghc822"
}:

pkgs.haskell.packages.${compiler}.developPackage {
  root = ./.;
  name = "brittany";
  overrides = with pkgs.haskell.lib; self: super: {
  };
  source-overrides = {
    ghc-exactprint = "0.5.8.0";
  };
}
