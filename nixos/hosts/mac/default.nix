{ pkgs, inputs, ...}:
let
  sources = pkgs.callPackage ../../_sources/generated.nix;
in {
  imports = [
    # inputs.plover-flake.homeManagerModules.plover
    # (import ../../home-manager/plover sources)
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

  home.packages = with pkgs; [
    kitty
    firefox
    google-chrome

    delta
    ghq
    just
    neovim
    tmux

    emacs-lsp-booster
    enchant
    emacsPackages.jinx # https://github.com/minad/jinx
    libtool
    # libvterm # FIXME: linux only

    as-tree
    eza
    fd
    ranger
    ripgrep
    xdg-ninja
 ];

  home.stateVersion = "25.05";
}
