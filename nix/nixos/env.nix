{
  config,
  pkgs,
  ...
}:
{
  nix = {
    settings = {
      auto-optimise-store = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
    };

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  # <https://nixos.wiki/wiki/Environment_variables>
  environment.sessionVariables = {
    EDITOR = "nvim -u NONE";
    BROWSER = "firefox";
    DEFAULT_BROWSER = "firefox";
    TERMINAL = "alacritty";

    XDG_CACHE_HOME = "\${HOME}/.cache";
    XDG_CONFIG_HOME = "\${HOME}/.config";
    XDG_BIN_HOME = "\${HOME}/.local/bin";
    XDG_DATA_HOME = "\${HOME}/.local/share";
    XDG_MUSIC_DIR = "/d/music/bandcamp";

    GTK_IM_MODULE = "fcitx";
    QT_IM_MODULE = "fcitx";
    XMODIFIERS = "@im=fcitx";
    GLFW_IM_MODULE = "ibus";

    PATH = [
      "\${XDG_BIN_HOME}"
    ];

    # any change?
    # https://github.com/NixOS/nixpkgs/issues/367836
    QT_QPA_PLATFORM = "xcb";
  };
}
