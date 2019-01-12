{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false, withHoogle ? true}:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;

  packageSet = if compiler == "default"
                 then pkgs.haskellPackages
                 else pkgs.haskell.packages.${compiler};

  haskellPackages = (
    if withHoogle
      then packageSet.override {
             overrides = (self: super:
               {
                 ghc = super.ghc // {
                   withPackages = (pkgfun:
                     super.ghc.withHoogle (pkgs: [ pkgs.hlint pkgs.hindent pkgs.hasktags ] ++ pkgfun pkgs)
                   );
                 };
                 ghcWithPackages = self.ghc.withPackages;
               }
             );
           }
      else packageSet
  );

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = haskellPackages.callPackage f {};

in
  if pkgs.lib.inNixShell then drv.env else drv
