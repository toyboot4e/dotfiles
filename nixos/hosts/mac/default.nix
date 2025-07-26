{ pkgs, inputs, ... }:
let
  sources = pkgs.callPackage ../../_sources/generated.nix;
  common-packages = import ../../home-manager/packages.nix pkgs;
in
{
  imports = [
    # inputs.plover-flake.homeManagerModules.plover
    # (import ../../home-manager/programs/plover sources)

    # (import ../../home-manager/programs/fish { inherit sources pkgs; })
    (import ../../home-manager/programs/emacs sources)
  ];

  home.packages =
    with pkgs;
    common-packages
    ++ [
      # macOS packages

      intel-one-mono
      emacs-lsp-booster
    ];

  home.stateVersion = "25.05";
}
