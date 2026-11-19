{
  config,
  pkgs,
  inputs,
  ...
}:
let
  sources = pkgs.callPackage ../../../_sources/generated.nix { };
in
{
  imports = [
    ../../home-manager/packages.nix
    ../../home-manager/links.nix
    ../../home-manager/programs/firefox
    (import ../../home-manager/programs/fish sources)
    (import ../../home-manager/programs/mpv sources)
    (import ../../home-manager/programs/emacs sources)
    (import ../../home-manager/programs/plover {
      home = config.home;
      inherit sources;
    })
  ];

  home.packages = with pkgs; [
    # macOS packages, etc.
    emacs-lsp-booster
    # claude-code
    # edge.claude-code
    # codex
  ];

  # Launchpad and Spotlight skip symlinked apps
  targets.darwin.linkApps.enable = false;
  targets.darwin.copyApps.enable = true;

  home.stateVersion = "25.05";
}
