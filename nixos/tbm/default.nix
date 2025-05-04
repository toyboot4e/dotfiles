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
      # package = pkgs.emacs-git;
      alwaysEnsure = true; # use nixpkgs
      extraEmacsPackages = import ./epkgs.nix { inherit pkgs sources; };
    };
  };

  # FIXME:
  # https://github.com/nix-community/home-manager/issues/2064
  systemd.user.targets.tray = {
    Unit = {
      Description = "Home Manager System Tray";
      Requires = [ "graphical-session-pre.target" ];
    };
  };

  home.packages = with pkgs; [
    appimage-run
    ncdu # check disk usage?

    kitty
    ghostty

    aider-chat

    convmv
    qutebrowser
    # firefox rofi
    google-chrome

    arandr
    bluetuith
    blueberry
    nemo
    nautilus
    xfce.thunar
    xdg-ninja
    neovim
    typora
    # helix

    nushell
    zellij

    act
    cargo-make # `makers`
    just
    hyperfine
    go-task

    # devbox
    # skk-dicts
    # geekbench
    fastfetch
    # meson
    ninja
    exiftool
    zip
    moreutils

    # emacs
    # emacsPackages.ddskk
    # TODO: not working?
    emacs-lsp-booster
    enchant
    emacsPackages.jinx # https://github.com/minad/jinx

    # CPU temperature
    lm_sensors

    gnumake
    cmake
    gcc
    gdb checksec
    # pwndbg 

    # gleam erlang rebar3
    # roswell

    go
    nodejs
    deno
    yarn
    volta
    python3
    # goenv
    # idris2.. using `idris2-pack` instead
    # https://github.com/stefan-hoeck/idris2-pack
    # chez
    # zig
    # zls
    # swiPrologWithGui

    # nix
    nix-search-tv
    television

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
    # qpdfview
    ghostscript
    pdfarranger

    # https://github.com/mkaz/termgraph
    python311Packages.termgraph
    # https://github.com/red-data-tools/YouPlot

    blender
    # cider

    # kicad-small
    # qmk

    # openai-whisper-cpp
    # (openai-whisper.override { cudaSupport = true; })
    # whisper-ctranslate2

    # espeak-classic
    # arcanPackages.espeak
    # python311Packages.pyttsx3

    ghc
    stack cabal-install haskell-language-server zlib ormolu
    haskellPackages.implicit-hie

    # purescript
    # ruby

    drawio
    krita

    # mdbook
    pandoc
    texlive.combined.scheme-full
    zoom-us
    # calibre
    minify

    # OCaml
    # ocaml
    # opam
    # dune_3
    # ocamlPackages.merlin

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
    # math-preview

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
    simplescreenrecorder
    zeal

    # DAW
    # reaper
    # bitwig-studio
    # yabridge
    # yabridgectl

    wineWowPackages.staging
    winetricks

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
