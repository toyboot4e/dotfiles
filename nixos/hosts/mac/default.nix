{ pkgs, inputs, ... }:
let
  sources = pkgs.callPackage ../../_sources/generated.nix;
  common-packages = import ../../home-manager/packages.nix pkgs;
in
{
  imports = [
    # inputs.plover-flake.homeManagerModules.plover
    # (import ../../home-manager/programs/plover sources)
    # TODO: switch
    # (import ../../home-manager/programs/fish  { inherit sources pkgs; })
  ];

  # programs.plover.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ../../../editor/emacs-leaf/init.org;
      # TODO: byte compilation
      # defaultInitFile = true;
      defaultInitFile = false;
      package = pkgs.emacs-unstable;
      # package = pkgs.emacs-git;
      alwaysEnsure = true; # use nixpkgs
      # extraEmacsPackages = import ./epkgs.nix { inherit pkgs sources; };
    };
  };

  home.packages =
    with pkgs;
    common-packages
    ++ [
      # macOS packages
    ];

  home.stateVersion = "25.05";
}
