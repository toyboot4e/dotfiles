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
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;

  codeUserDir = if isDarwin then "Library/Application Support/Code/User" else ".config/Code/User";
  qutebrowserDir = if isDarwin then ".qutebrowser" else ".config/qutebrowser";
in
{
  home.file = {
    ".bashrc" = link "shell/bash/.bashrc";
    ".bash_profile" = link "shell/bash/.bash_profile";
    ".emacs.d" = link "editor/emacs-leaf";

    ".w3m" = link "browser/w3m";

    # Only these; the rest of `~/.claude` is state
    ".claude/CLAUDE.md" = link "tool/claude/CLAUDE.md";
    ".claude/settings.json" = link "tool/claude/settings.json";
    ".claude/hooks" = link "tool/claude/hooks";

    "${codeUserDir}/settings.json" = link "editor/vscode/settings.json";
    "${codeUserDir}/keybindings.json" = link "editor/vscode/keybindings.json";

    "${qutebrowserDir}" = link "browser/qutebrowser";
  };

  xdg.configFile = lib.mkMerge [
    {
      "alacritty" = link "terminal/alacritty";
      "kitty" = link "terminal/kitty";
      "fish/functions" = link "shell/fish/functions";
      "fish/completions" = link "shell/fish/completions";
      "fish/conf.d/fish_frozen_theme.fish" = link "shell/fish/conf.d/fish_frozen_theme.fish";
      "tmux" = link "tool/tmux";
      # Per file: the Claude Code sandbox cannot read a symlinked directory
      "git/config" = link "tool/git/config";
      "git/ignore" = link "tool/git/ignore";
      "gh" = link "tool/gh";
      "bat" = link "tool/bat";
      "cargo" = link "tool/cargo";
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
