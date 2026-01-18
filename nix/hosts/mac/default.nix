{
  config,
  pkgs,
  inputs,
  ...
}:
let
  sources = pkgs.callPackage ../../_sources/generated.nix;
  common-packages = import ../../home-manager/packages.nix pkgs;
in
{
  imports = [
    (import ../../home-manager/programs/emacs sources)

    inputs.plover-flake.homeManagerModules.plover
    (import ../../home-manager/programs/plover {
      home = config.home;
      inherit sources;
    })
  ];

  home.packages =
    with pkgs;
    common-packages
    ++ [
      # macOS packages, etc.
      emacs-lsp-booster
      # claude-code
      # edge.claude-code
      # codex
    ];

  home.stateVersion = "25.05";
}
