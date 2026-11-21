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
    ../../home-manager/programs/tmux
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

  # Commit signing via `ssh-keygen -Y sign` never triggers `AddKeysToAgent`, so preload
  # keys whose passphrases were stored with `/usr/bin/ssh-add --apple-use-keychain`
  launchd.agents.ssh-add-keychain = {
    enable = true;
    config = {
      ProgramArguments = [
        "/usr/bin/ssh-add"
        "--apple-load-keychain"
      ];
      RunAtLoad = true;
    };
  };

  # Launchpad and Spotlight skip symlinked apps
  targets.darwin.linkApps.enable = false;
  targets.darwin.copyApps.enable = true;

  home.stateVersion = "25.05";
}
