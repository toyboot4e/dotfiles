{ pkgs, inputs, ...}:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ../../editor/emacs-leaf/init.org;
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

    just
    neovim

    emacs-lsp-booster
    enchant
    emacsPackages.jinx # https://github.com/minad/jinx
    libtool
    # libvterm # FIXME: linux only

    eza
    fd
    ranger
    ripgrep
 ];

  home.stateVersion = "25.05";
}
