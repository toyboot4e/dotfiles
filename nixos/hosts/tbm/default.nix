# `home-manager` configuraiton
{ pkgs, inputs, ... }:
let
  sources = pkgs.callPackage ../../_sources/generated.nix;
  common-packages = import ../../home-manager/packages.nix pkgs;
in
{
  imports = [
    inputs.plover-flake.homeManagerModules.plover
    (import ../../home-manager/programs/plover sources)
    (import ../../home-manager/programs/emacs sources)
    ./desktop.nix
    ./de/x.nix
    ./de/wayland.nix
    ./input-mozc.nix
    ./services.nix
    ./virtual.nix
  ];

  # FIXME: This will soon not be possible. Please remove all `nixpkgs` options when using `home-manager.useGlobalPkgs`.
  nixpkgs.config.allowUnfree = true;

  # boot.kernelPackages = pkgs.linuxPackages_latest;

  # FIXME:
  # https://github.com/nix-community/home-manager/issues/2064
  systemd.user.targets.tray = {
    Unit = {
      Description = "Home Manager System Tray";
      Requires = [ "graphical-session-pre.target" ];
    };
  };

  home.packages =
    with pkgs;
    common-packages
    ++ [
      emacs-lsp-booster

      appimage-run
      aider-chat

      # Not on macOS
      obs-studio
      xdot

      # pwn
      checksec
      # pwndbg

      # Translation
      poedit

      # online-judge-tools
      # my-pkgs.online-judge-verify-helper

      arandr
      bluetuith
      blueberry
      nemo
      nautilus
      xfce.thunar

      # devbox
      # skk-dicts
      # geekbench
      # meson
      ninja
      exiftool
      zip
      moreutils

      # Emacs
      libvterm

      # CPU temperature
      lm_sensors

      # goenv
      # idris2.. using `idris2-pack` instead
      # https://github.com/stefan-hoeck/idris2-pack
      # chez
      # zig
      # zls
      # swiPrologWithGui

      sway-scratch

      # docker
      readline
      rlwrap
      sqlite-interactive
      sqlite-web
      sqlite-utils
      evince
      # qpdfview
      ghostscript
      pdfarranger

      # https://github.com/mkaz/termgraph
      # python311Packages.termgraph
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

      # purescript
      # ruby

      # mdbook
      texlive.combined.scheme-full
      # calibre
      minify

      # TODO: replace `sxhkd` package with `sxhkd` service
      # sxhkd

      # GUI
      vkmark
      steamtinkerlaunch
      simplescreenrecorder

      # DAW
      # reaper
      # bitwig-studio
      # yabridge
      # yabridgectl

      # wineWowPackages.staging
      # winetricks

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
