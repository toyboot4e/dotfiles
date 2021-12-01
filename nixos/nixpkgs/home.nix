{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "tbm";
  home.homeDirectory = "/Users/tbm";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  # TODO: build python-dependent packages on macOS
  home.packages = with pkgs; [
    # apps
    alacritty w3m tig

    # system
    python2 python3
    openssh

    # build
    # make
    cmake ninja llvm
    ffmpeg imagemagick

    # deps
    SDL

    # JS
    nodejs yarn emscripten

    # TUI
    git gh ghq
    tealdeer zoxide
    ranger cmus

    # alternatives
    fd ripgrep as-tree fzf exa bat
    tokei

    # monitor
    dust htop
    # bat-extras

    # infra
    asciidoctor ditaa
    mpv youtube-dl # bandcamp-dl

    # languages
    ccls
    rustup rust-analyzer sccache
    go sbcl

    # more filters
    jq pup
  ];

  # System:
  # bash dash fish tmux
  # kitty qutebrowser

  # macOS:
  # brew install \
  #  kitty qutebrowser \
  #  bandcamp-dl \
  #  skhd yabai \
  #  nvim emacs \
  #  zld \
  # omnisharp-roslyn

  # GNU-sed
}
