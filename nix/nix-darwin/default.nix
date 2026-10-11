# Base system shared by all the macOS users. Per-user entries go to `nix/hosts/<user>/darwin.nix`
{ pkgs, user, ... }:
let
  sources = pkgs.callPackage ../../_sources/generated.nix { };
in
{
  # networking.hostName = host;
  nixpkgs.hostPlatform = "aarch64-darwin"; # FIXME: take it from somewhere..
  # nixpkgs.hostPlatform = forAllSystems(pkgs: pkgs.stdenv.hostPlatform.system);
  nixpkgs.config.allowUnfree = true;
  system.primaryUser = user;

  nix = {
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      sandbox = true;
    };

    optimise.automatic = true;

    gc = {
      automatic = true;
      interval = [ { Weekday = 7; } ];
      options = "--delete-older-than 7d";
    };
  };

  environment.shells = [
    pkgs.bash
    pkgs.fish
  ];

  environment.variables.SHELL = "/bin/bash";
  environment.variables.EDITOR = "nvim";

  fonts.packages = [
    # pkgs.intel-one-mono
    pkgs.nerd-fonts.intone-mono
    pkgs.qmk

    # Because Kitty has builtin Nerd Font, we should use vanilla fonts:
    # https://sw.kovidgoyal.net/kitty/faq/#kitty-is-not-able-to-use-my-favorite-font
    # pkgs.nerd-fonts.intone-mono
  ];

  # set fish shell
  # https://github.com/nix-darwin/nix-darwin/issues/1237
  # TODO: can we use home-manager?
  programs.bash.enable = true;
  programs.fish.enable = true;
  # workaround: https://github.com/nix-community/home-manager/issues/8435
  programs.fish.useBabelfish = true;

  # NOTE: Homebrew itself has to be installed manually
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
    };

    taps = [
      # "d12frosted/emacs-plus"
      "FelixKratz/formulae" # sketchy bar
      "oven-sh/bun"
      "nikitabobko/tap" # aerospace
      "mediosz/tap" # swipeaerospace
    ];

    brews = [
      "awscli"
      "oven-sh/bun/bun"
      # "emacs-plus"
      "FelixKratz/formulae/sketchybar"
      "fontconfig"
      "libvterm"
      # # TODO: limit to mp, on write it in flake.nix
      # "ios-deploy"
      # "libmagic"
      # "redis"
      # "grpc"
    ];

    casks = [
      "1password-cli"
      "alacritty"
      "claude-code@latest"
      "coteditor"
      "docker-desktop"
      "drawio"
      "firefox"
      "font-hack-nerd-font"
      "google-chrome"
      "karabiner-elements"
      "mediosz/tap/swipeaerospace"
      "nikitabobko/tap/aerospace"
      "session-manager-plugin"
      "slack"
      "tableplus"
      # "qt-creator"
      # "qutebrowser"
    ];
  };

  # SSH: allow remote login from other machines on the LAN
  services.openssh.enable = true;

  users.knownUsers = [ user ];
  users.users.${user} = {
    shell = pkgs.fish;
    uid = 501;
    openssh.authorizedKeys.keys =
      let
        sshKeys = import ../../ssh-keys.nix;
        allKeys = builtins.attrValues sshKeys;
      in
      # authorize every key except the host's own
      builtins.filter (k: k != sshKeys.${user}) allKeys;
  };

  # environment.systemPackages = with pkgs; [];

  launchd.user.agents.swipeaerospace.serviceConfig = {
    ProgramArguments = [
      "/usr/bin/open"
      "-a"
      "SwipeAeroSpace"
    ];
    RunAtLoad = true;
  };

  # Dock reads the -currentHost copy of trackpad gestures, which `system.defaults` can't write
  system.activationScripts.postActivation.text = ''
    sudo -u ${user} defaults -currentHost write -g com.apple.trackpad.threeFingerHorizSwipeGesture -int 0
    sudo -u ${user} defaults -currentHost write -g com.apple.trackpad.threeFingerVertSwipeGesture -int 0
  '';

  system = {
    defaults = {
      NSGlobalDomain = {
        AppleShowAllExtensions = true;
        AppleEnableSwipeNavigateWithScrolls = false;
        NSAutomaticWindowAnimationsEnabled = false;
        NSWindowResizeTime = 0.001;
      };
      universalaccess.reduceMotion = false;
      # 3-finger horizontal swipe is taken by SwipeAeroSpace
      trackpad.TrackpadThreeFingerHorizSwipeGesture = 0;
      trackpad.TrackpadThreeFingerVertSwipeGesture = 0;
      CustomUserPreferences."com.apple.dock".showMissionControlGestureEnabled = false;
      # Leave these keys to AeroSpace: "Switch to Desktop 1-9" (ctrl-1..9),
      # Spotlight (cmd-space) and Finder search (cmd-alt-space).
      # nix-darwin overwrites this whole dict, so shortcuts set in System Settings are lost on switch
      CustomUserPreferences."com.apple.symbolichotkeys".AppleSymbolicHotKeys =
        builtins.listToAttrs (
          map (id: {
            name = toString id;
            value.enabled = false;
          }) (builtins.genList (i: 118 + i) 9)
        )
        // {
          "64" = {
            enabled = false;
            value = {
              parameters = [
                32
                49
                1048576
              ];
              type = "standard";
            };
          };
          "65" = {
            enabled = false;
            value = {
              parameters = [
                32
                49
                1572864
              ];
              type = "standard";
            };
          };
          # Apps (formerly Launchpad): cmd-e
          "160" = {
            enabled = true;
            value = {
              parameters = [
                101
                14
                1048576
              ];
              type = "standard";
            };
          };
        };
      # AeroSpace parks hidden windows at a monitor corner; per-display Spaces let them leak onto neighbors
      spaces.spans-displays = true;
      finder = {
        AppleShowAllFiles = true;
        AppleShowAllExtensions = true;
      };
      dock = {
        autohide = true;
        mru-spaces = false; # don't reorder (Most Recently Used spaces)
        show-recents = false;
        orientation = "bottom";
        expose-group-apps = true;
      };
    };
  };

  environment.variables = {
    TERMINAL = "alacritty";

    XDG_CACHE_HOME = "\${HOME}/.cache";
    XDG_CONFIG_HOME = "\${HOME}/.config";
    XDG_BIN_HOME = "\${HOME}/.local/bin";
    XDG_DATA_HOME = "\${HOME}/.local/share";
    XDG_MUSIC_DIR = "/d/music/bandcamp";
  };

  system.stateVersion = 6;
}
