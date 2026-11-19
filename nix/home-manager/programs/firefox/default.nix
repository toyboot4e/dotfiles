{
  config,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.programs.firefox;
  profile = "default";
  addons = inputs.firefox-addons.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  programs.firefox = {
    enable = true;
    # Installed as a Homebrew cask: macOS 27 denies the unsigned nixpkgs build access
    # to the default datadir, surfacing as "A copy of Firefox is already open"
    # https://github.com/NixOS/nixpkgs/issues/535123
    package = null;
    profiles.${profile} = {
      id = 0;
      isDefault = true;
      settings = {
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "extensions.autoDisableScopes" = 0;
      };
      extensions = {
        packages = with addons; [
          tree-style-tab
          vimium
        ];
        # Seeds storage.local only once per profile; Firefox migrates it into IndexedDB.
        # Vimium reads storage.sync, so restore ../../../../browser/firefox/vimium-options.json
        # from its options page instead.
        settings."treestyletab@piro.sakura.ne.jp" = {
          force = true;
          settings = {
            autoCollapseExpandSubtreeOnAttach = false;
            autoCollapseExpandSubtreeOnSelect = false;
            fixupTreeOnTabVisibilityChanged = true;
            labelOverflowStyle = "crop";
            showExpertOptions = true;
            sidebarPosition = 1;
            skipCollapsedTabsForTabSwitchingShortcuts = true;
            style = "photon";
            tabPreviewTooltip = true;
            # Left unversioned so TST's migration moves this into its base64 chunkedUserStyleRules
            userStyleRules = builtins.readFile ../../../../browser/firefox/chrome/tree_style_tab.css;
          };
        };
      };
    };
  };

  # Without it Firefox picks the profile from installs.ini (keyed by app path) and
  # tries to write an [Install<hash>] section into HM's read-only profiles.ini
  launchd.agents.firefox-legacy-profiles = {
    enable = true;
    config = {
      ProgramArguments = [
        "/bin/launchctl"
        "setenv"
        "MOZ_LEGACY_PROFILES"
        "1"
      ];
      RunAtLoad = true;
    };
  };

  # Out-of-store so CSS edits apply on Firefox restart without `switch`
  home.file."${cfg.profilesPath}/${profile}/chrome".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/browser/firefox/chrome";
}
