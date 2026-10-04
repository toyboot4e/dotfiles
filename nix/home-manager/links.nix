# Out-of-store symlinks into this repository, editable without `switch`
{
  config,
  lib,
  pkgs,
  ...
}:
let
  # Must be a string, not a path literal; a path would be copied into the store
  root = "${config.home.homeDirectory}/dotfiles";
  link = path: { source = config.lib.file.mkOutOfStoreSymlink "${root}/${path}"; };
  inherit (pkgs.stdenv) isDarwin isLinux;

  codeUserDir = if isDarwin then "Library/Application Support/Code/User" else ".config/Code/User";
  qutebrowserDir = if isDarwin then ".qutebrowser" else ".config/qutebrowser";
in
{
  home.file = {
    ".bashrc" = link "shell/bash/.bashrc";
    ".bash_profile" = link "shell/bash/.bash_profile";
    ".emacs.d" = link "editor/emacs-leaf";

    ".w3m" = link "browser/w3m";

    "${codeUserDir}/settings.json" = link "editor/vscode/settings.json";
    "${codeUserDir}/keybindings.json" = link "editor/vscode/keybindings.json";

    "${qutebrowserDir}" = link "browser/qutebrowser";
  };

  xdg.configFile = lib.mkMerge [
    {
      "alacritty" = link "terminal/alacritty";
      "kitty" = link "terminal/kitty";
      "fish" = link "shell/fish";
      "tmux" = link "tool/tmux";
      "git" = link "tool/git";
      "gh" = link "tool/gh";
      "bat" = link "tool/bat";
      "cargo" = link "tool/cargo";
      "mise" = link "tool/mise";
      "mpv" = link "tool/mpv";
      "ranger" = link "tool/ranger";
      "vim" = link "editor/vim";
      "nvim" = link "editor/nvim";
    }

    (lib.mkIf isDarwin {
      "aerospace" = link "macos/aerospace";
      "sketchybar" = link "macos/sketchybar";
      "karabiner" = link "macos/karabiner";
      # SwipeAeroSpace itself reads this misspelled directory
      "swipeareospace" = link "macos/swipeaerospace";
    })

    (lib.mkIf isLinux {
      "rofi" = link "linux/rofi";
      "i3" = link "linux/i3";
      "i3blocks" = link "linux/i3blocks";
      "sway" = link "linux/sway";
      "sxhkd" = link "linux/sxhkd";
    })
  ];
}
