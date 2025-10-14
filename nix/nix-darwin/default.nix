host:
{ pkgs, ... }:
let
  sources = pkgs.callPackage ../_sources/generated.nix { };
in
{
  # networking.hostName = host;
  nixpkgs.config.allowUnfree = true;
  system.primaryUser = host;

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
    pkgs.intel-one-mono

    # Because Kitty has builtin Nerd Font, we should use vanilla fonts:
    # https://sw.kovidgoyal.net/kitty/faq/#kitty-is-not-able-to-use-my-favorite-font
    # pkgs.nerd-fonts.intone-mono
  ];

  # set fish shell
  # https://github.com/nix-darwin/nix-darwin/issues/1237
  # TODO: can we use home-manager?
  programs.bash.enable = true;
  programs.fish.enable = true;

  # NOTE: Homebrew itself has to be installed manually
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      # WARNING: It deletes homebrew packages not installed via Nix
      cleanup = "uninstall";
    };
    taps = [
      # "d12frosted/emacs-plus"
      "FelixKratz/formulae" # sketchy bar
      "koekeishiya/formulae" # yabai
    ];
    brews = [
      "awscli"
      # "emacs-plus"
      "FelixKratz/formulae/sketchybar"
      "fontconfig"
      "ghcup"
      "koekeishiya/formulae/yabai"
      "koekeishiya/formulae/skhd"
      "libvterm"
      # TODO: limit to mp (separate and merge later)
      "anyenv"
      "ios-deploy"
      "libmagic"
      "redis"
    ];
    casks = [
      "coteditor"
      "docker"
      "font-hack-nerd-font" # the default font of sketchy bar
      "karabiner-elements"
      "qutebrowser"
      # TODO: limit to mp (separate and merge later)
      "android-commandlinetools"
      "android-platform-tools"
      "android-studio"
      "google-chrome"
      "chromedriver"
    ];
    extraConfig = ''
      brew "yabai", env: { "SHELL" => "/bin/bash" }
      brew "skhd", env: { "SHELL" => "/bin/bash" }
    '';
  };

  users.knownUsers = [ host ];
  users.users.${host} = {
    shell = pkgs.fish;
    uid = 501;
  };

  # TODO: really need this?
  environment.systemPackages = with pkgs; [
    # skhd
    # yabai
  ];

  # https://github.com/nix-darwin/nix-darwin/issues/1041
  # services.karabiner-elements = {
  #   enable = true;
  #   package = pkgs.karabiner-elements.overrideAttrs (old: {
  #     version = "14.13.0";
  #
  #     src = pkgs.fetchurl {
  #       inherit (old.src) url;
  #       hash = "sha256-gmJwoht/Tfm5qMecmq1N6PSAIfWOqsvuHU8VDJY8bLw=";
  #     };
  #
  #     dontFixup = true;
  #   });
  # };

  # services.skhd.enable = true;
  # services.yabai = {
  #   enable = true;
  #   package = pkgs.yabai;
  #   enableScriptingAddition = true;
  #   extraConfig = ''
  #     # sudo yabai --load-sa
  #     # yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
  #     sudo yabai
  #     yabai -m signal --add event=dock_did_restart action="sudo yabai"
  #   '';
  # };

  system = {
    defaults = {
      NSGlobalDomain.AppleShowAllExtensions = true;
      finder = {
        AppleShowAllFiles = true;
        AppleShowAllExtensions = true;
      };
      dock = {
        autohide = true;
        show-recents = false;
        orientation = "bottom";
      };
    };
  };

  system.stateVersion = 6;
}
