# Shared between macOS and NixOS
pkgs: with pkgs; [
  # --------------------------------------------------------------------------------
  # Terminal
  # --------------------------------------------------------------------------------

  # terminal
  kitty
  tmux

  # text editors
  neovim

  # Emacs
  enchant
  # emacsPackages.jinx # https://github.com/minad/jinx
  libtool

  git
  delta
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
  eza
  fd
  ranger
  rename
  ripgrep

  # filters
  jq
  pup

  # encoding
  nkf

  # dotfiles
  fastfetch
  xdg-ninja

  # nix
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
  google-chrome
  # qutebrowser

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
  # FIXME: use pylsp installed with uv locally
  python3Packages.python-lsp-server
  uv

  # monitoring
  ncdu
  gtop

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
