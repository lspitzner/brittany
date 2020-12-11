{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/nixos/nixpkgs/archive/069f183f16c3ea5d4b6e7625433b92eba77534f7.tar.gz"; # nixos-unstable
  sha256 = "1by9rqvr2k6iz2yipf89yaj254yicpwq384ijgyy8p71lfxbbww2";
}, pkgs ? import nixpkgsSrc { }, compiler ? null, forShell ? pkgs.lib.inNixShell
}:

let
  haskellPackages = if compiler == null then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  name = "brittany";
  root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  returnShellEnv = forShell;
}
