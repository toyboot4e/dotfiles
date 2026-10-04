{
  config,
  pkgs,
  inputs,
  ...
}:
let
  sources = pkgs.callPackage ../../../_sources/generated.nix { };
  common-packages = import ../../home-manager/packages.nix pkgs;
in
{
  imports = [
    ../../home-manager/links.nix
    (import ../../home-manager/programs/fish sources)
    (import ../../home-manager/programs/mpv sources)
    (import ../../home-manager/programs/emacs sources)
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
