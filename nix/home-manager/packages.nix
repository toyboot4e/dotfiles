# Shared between macOS and NixOS
pkgs: with pkgs; [
  # --------------------------------------------------------------------------------
  # Terminal
  # --------------------------------------------------------------------------------

  # terminal
  kitty
  tmux

  direnv
  nix-direnv
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
  hyperfine
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
  pup

  # formatters
  nodePackages.prettier

  # encoding
  nkf

  # dotfiles
  fastfetch
  xdg-ninja

  # nix
  nil # Nix LSP: https://github.com/oxalica/nil
  nixfmt-rfc-style
  nix-search-tv
  nvfetcher
  rippkgs
  television

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

  # movies
  mpv

  # SNS
  discord
  slack
  zoom-us

  # drawing
  drawio
  gimp
  inkscape

  # --------------------------------------------------------------------------------
  # Languages
  # --------------------------------------------------------------------------------

  # C
  cmake
  gcc
  gdb

  # Go
  go

  # Haskell
  ghc
  stack
  cabal-install
  haskell-language-server
  zlib
  ormolu
  haskellPackages.implicit-hie

  # JS
  # deno
  nodejs
  volta
  yarn

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
  ])

  # Python
  python3
  virtualenv
  # FIXME: use pylsp installed with uv locally
  python3Packages.python-lsp-server
  uv
  ansible

  # YAML
  yamlfmt

  # monitoring
  ncdu
  gtop

  # --------------------------------------------------------------------------------
  # Typesetting or SSG
  # --------------------------------------------------------------------------------

  # Tex
  texlive.combined.scheme-full
  minify
  # mdbook

  typst
  tinymist

  # --------------------------------------------------------------------------------
  # Behind the scenes
  # --------------------------------------------------------------------------------

  # CI
  actionlint
  act

  # documentation
  ditaa
  gnuplot
  graphviz
  jdk
  mermaid-cli
  pandoc
  plantuml
]
