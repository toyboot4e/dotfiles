# `home-manager` configuraiton
{pkgs, ...}:
let sources = pkgs.callPackage ./_sources/generated.nix { };
in {
  imports = [
    ./desktop.nix
    ./input-mozc.nix
    ./services.nix
    ./virtual.nix
  ];

  nixpkgs.config.allowUnfree = true;
  # boot.kernelPackages = pkgs.linuxPackages_latest;

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ../../editor/emacs-leaf/init.org;
      # TODO: byte compilation
      # defaultInitFile = true;
      defaultInitFile = false;
      package = pkgs.emacs-unstable;
      alwaysTangle = true;
      alwaysEnsure = true;
      extraEmacsPackages = import ./epkgs.nix { inherit pkgs sources; };
    };
  };

  home.packages = with pkgs; [
    # ----------------------------------------------------------------------------------------------------
    kitty
    convmv
    qutebrowser
    # firefox rofi
    google-chrome

    arandr
    bluetuith
    blueberry
    cinnamon.nemo
    nautilus
    xfce.thunar
    xdg-ninja
    neovim
    helix

    nushell
    devbox
    skk-dicts
    # geekbench
    fastfetch
    meson
    ninja
    exiftool
    zip
    moreutils

    # emacs
    # emacsPackages.ddskk
    # TODO: not working?
    emacs-lsp-booster

    # CPU temperature
    lm_sensors

    gnumake
    cmake
    gcc
    gdb pwndbg checksec
    go
    nodejs
    deno
    yarn
    volta
    python3
    roswell
    # goenv
    # idris2.. using `idris2-pack` instead
    # https://github.com/stefan-hoeck/idris2-pack
    chez
    zig
    zls
    alejandra

    nvfetcher
    (fenix.complete.withComponents ["cargo" "clippy" "rust-src" "rustc" "rustfmt"])
    # rust-analyzer-nightly

    # koka

    # docker
    readline
    rlwrap
    sqlite-interactive
    sqlite-web
    sqlite-utils
    slack
    zulip
    vscode
    mpv
    gimp
    evince
    qpdfview
    ghostscript
    pdfarranger

    # https://github.com/mkaz/termgraph
    python311Packages.termgraph
    # https://github.com/red-data-tools/YouPlot

    blender
    cider

    kicad-small
    qmk

    # (openai-whisper.override { cudaSupport = true; })
    openai-whisper-cpp
    # whisper-ctranslate2

    espeak-classic
    # arcanPackages.espeak
    # python311Packages.pyttsx3

    ghc
    stack
    cabal-install
    haskell-language-server
    zlib
    haskellPackages.implicit-hie
    ormolu
    ruby
    drawio
    mdbook
    pandoc
    texlive.combined.scheme-full
    # calibre
    minify

    # OCaml
    ocaml
    opam
    dune_3
    ocamlPackages.merlin

    pup
    jq
    watchexec
    rename
    jdk
    ditaa
    graphviz
    xdot
    mermaid-cli
    plantuml
    nkf
    gnuplot

    cmatrix
    figlet

    # TODO: replace `sxhkd` package with `sxhkd` service
    # sxhkd

    # GUI
    discord
    vkmark
    unityhub
    steamtinkerlaunch
    obs-studio
    zeal

    # DAW
    # reaper
    bitwig-studio
    wineWowPackages.staging
    winetricks
    yabridge
    yabridgectl

    # Web
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";
}
