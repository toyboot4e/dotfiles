{ pkgs, inputs, ... }:
let
  sources = pkgs.callPackage ../../_sources/generated.nix;
  common-packages = import ../../home-manager/packages.nix pkgs;
  emacs-packages = pkgs.emacs.override {
    withNativeCompilation = false;
  };
in
{
  imports = [
    # inputs.plover-flake.homeManagerModules.plover
    # (import ../../home-manager/programs/plover sources)
    # TODO: switch
    # (import ../../home-manager/programs/fish { inherit sources pkgs; })
    # (import ../../home-manager/programs/emacs sources)
  ];

  # programs.plover.enable = true;

  home.packages =
    with pkgs;
    common-packages
    ++ [
      # macOS packages
      emacs-packages
      # emacs-lsp-booster
    ];

  home.stateVersion = "25.05";
}
