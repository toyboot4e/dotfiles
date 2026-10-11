sources:
{ lib, pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      # TODO does it work? (`.el`?)
      config = ../../../../editor/emacs-leaf/init.org;
      # TODO: byte compilation
      # defaultInitFile = true;
      defaultInitFile = false;
      # package = pkgs.emacs-unstable;
      # package = pkgs.emacs-git;
      # package = pkgs.emacs;
      package = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs31 else pkgs.emacs-unstable;
      alwaysEnsure = true; # use nixpkgs
      extraEmacsPackages = import ./epkgs.nix { inherit pkgs sources; };
    };
  };
}
