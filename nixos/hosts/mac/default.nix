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
    # (import ../../home-manager/programs/fish { inherit sources pkgs; })
    # (import ../../home-manager/programs/emacs sources)
  ];

  # programs.plover.enable = true;

  home.packages =
    with pkgs;
    common-packages
    ++ [
      # macOS packages

      # None of them is working correctly
      # emacs-lsp-booster
      # (emacs.override { withNativeCompilation = false; })
    ];

  home.stateVersion = "25.05";
}
