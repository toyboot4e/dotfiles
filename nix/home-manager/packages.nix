# Shared between macOS and NixOS
{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # --------------------------------------------------------------------------------
    # Terminal
    # --------------------------------------------------------------------------------

    # terminal
    # kitty
    alacritty
    tmux

    # dev env
    direnv
    nix-direnv
    # mise

    # commands
    zoxide
    tealdeer

    # text editors
    neovim

    # Emacs
    enchant
    # emacsPackages.jinx # https://github.com/minad/jinx
    libtool

    git
    delta
    diff-so-fancy
    gh
    ghq

    # build tools
    gnumake
    # hyperfine
    just
    watchexec

    # --------------------------------------------------------------------------------
    # CLI tools
    # --------------------------------------------------------------------------------

    # fuzzy finders
    fzf

    # utilities
    as-tree
    bat
    eza
    fd
    ranger
    rename
    ripgrep
    tokei

    # filters
    jq
    yq-go
    pup

    # general linters
    # codebook

    # encoding
    nkf

    # nix
    nil # Nix LSP: https://github.com/oxalica/nil
    nixfmt
    # nix-search-tv
    nvfetcher
    rippkgs
    # television

    # ascii art
    cmatrix
    figlet

    # --------------------------------------------------------------------------------
    # GUI
    # --------------------------------------------------------------------------------

    # browsers
    firefox
    # google-chrome
    # chromedriver
    # qutebrowser # not available on macOS

    # text editors
    vscode

    # SNS
    zoom-us

    # drawing
    # inkscape

    # --------------------------------------------------------------------------------
    # Languages
    # --------------------------------------------------------------------------------

    # C
    cmake
    gcc
    # gdb

    # JS
    # deno
    ni

    # OCaml
    # ocaml
    # opam
    # dune_3
    # ocamlPackages.merlin

    # Rust
    (fenix.complete.withComponents [
      "cargo"
      "clippy"
      "rust-src"
      "rustc"
      "rustfmt"
      "rust-analyzer"
    ])

    # Python
    python3
    ty
    # FIXME: ansible broke build on mp(I forgot where)
    # ansible

    # YAML
    yamlfmt

    # monitoring
    ncdu
    gtop

    # --------------------------------------------------------------------------------
    # Typesetting or SSG
    # --------------------------------------------------------------------------------

    # Tex
    texliveSmall
    minify
    # mdbook

    typst
    tinymist

    # --------------------------------------------------------------------------------
    # Image processing
    # --------------------------------------------------------------------------------

    ffmpeg
    imagemagick
    ghostscript

    # --------------------------------------------------------------------------------
    # Behind the scenes
    # --------------------------------------------------------------------------------

    # documentation
    ditaa
    gnuplot
    graphviz
    jdk
    mermaid-cli
    plantuml
  ];
}
