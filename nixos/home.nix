# `home-manager` configuraiton

{ config, pkgs, ... }:

let
  # unstable = import <unstable> { config = { allowUnfree = true; }; };
  # local = import "/home/tbm/ghq/github.com/nix-community/home-manager";
in

{
  nixpkgs.config.allowUnfree = true;
  # boot.kernelPackages = pkgs.linuxPackages_latest;

  # FIXME: local?
  # programs.home-manager.path = "/home/tbm/ghq/github.com/nix-community/home-manager";

  # overlays
  # <https://github.com/nix-community/home-manager/issues/1107>
  nixpkgs.overlays = [
    # fenix: <https://github.com/nix-community/fenix>
    (import "${fetchTarball "https://github.com/nix-community/fenix/archive/main.tar.gz"}/overlay.nix")
  ];

  # auto mount: <https://nix-community.github.io/home-manager/options.html#opt-services.udiskie.enable>
  services.udiskie = {
    enable = true;
    automount = true;
    notify = false;
  };

  # TODO: trying `fcitx5` from `home-manager` (is `QT_PLUGIN_PATH` exported?)
  i18n.inputMethod = {
    enabled = "fcitx5";
    fcitx5.addons = with pkgs; [ fcitx5-mozc fcitx5-gtk ];
  };

  # Use `sxhkd` service, but without overwriting the configuration file
  # - sxhkd: <https://nix-community.github.io/home-manager/options.html#opt-services.sxhkd.enable>
  # - configFile.<name>.enable: <https://rycee.gitlab.io/home-manager/options.html#opt-xdg.configFile._name_.enable>
  services.sxhkd.enable = true;
  xdg.configFile."sxhkd/sxhkdrc".enable = false;

  # TODO: change cursor
  # home.pointerCursor = 
  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.gnome.adwaita-icon-theme;
    # size = 24;
    size = 32;
  };

  xdg.mimeApps = {
    enable = true;

    associations.added = {
      "application/pdf" = ["org.gnome.Evince.desktop"];
    };

    defaultApplications = {
      "application/pdf" = ["org.gnome.Evince.desktop"];

      # FIXME: not working correctly
      "x-scheme-handler/http" = "org.firefox.firefox.desktop";
      "x-scheme-handler/https" = "org.firefox.firefox.desktop";
      "x-scheme-handler/about" = "org.firefox.firefox.desktop";
      "x-scheme-handler/unknown" = "org.firefox.firefox.desktop";
    };
  };

  # Let `home-manager` overwrite `mimeapps.list` so that it doesn't fail:
  xdg.configFile."mimeapps.list".force = true;

  # NOTE: You have to let `home-manager` manage your shell config file, or the
  # `home.sessionVariables` does NOT take effect.
  # home.sessionVariables = {
  #   EDITOR = "nvim";
  #   BROWSER = "firefox";
  #   TERMINAL = "kitty";
  # };

  home.packages = with pkgs; [
    # ----------------------------------------------------------------------------------------------------
    kitty
    convmv
    qutebrowser
    # firefox rofi
    google-chrome

    arandr bluetuith blueberry
    cinnamon.nemo gnome.nautilus xfce.thunar
    xdg-ninja emacs neovim helix
    # geekbench
    neofetch meson ninja
    exiftool
    zip

    # CPU temperature
    lm_sensors

    gnumake cmake gcc go nodejs deno yarn python3 roswell
    # goenv
    # idris2.. using `idris2-pack` instead
    # https://github.com/stefan-hoeck/idris2-pack
    chez zig zls

    #### # rustup # TODO: `rustup componend add` does not add ~/.cargo/bin/* symlinks
    #### # rustc cargo rustfmt clippy rust-analyzer
    (fenix.complete.withComponents [ "cargo" "clippy" "rust-src" "rustc" "rustfmt" ])
    rust-analyzer-nightly

    koka

    # docker
    readline rlwrap
    sqlite-interactive sqlite-web sqlite-utils
    slack zulip vscode mpv gimp evince

    # https://github.com/mkaz/termgraph
    python311Packages.termgraph
    # https://github.com/red-data-tools/YouPlot

    blender

    # (openai-whisper.override { cudaSupport = true; })
    openai-whisper-cpp
    # whisper-ctranslate2

    espeak-classic
    # arcanPackages.espeak
    # python311Packages.pyttsx3

    ghc stack cabal-install haskell-language-server zlib haskellPackages.implicit-hie ormolu
    ruby
    drawio mdbook pandoc texlive.combined.scheme-full calibre minify

    pup jq watchexec
    rename
    jdk ditaa graphviz plantuml
    nkf gnuplot

    cmatrix figlet

    # TODO: replace `sxhkd` package with `sxhkd` service
    # sxhkd

    # GUI
    discord vkmark unityhub steamtinkerlaunch obs-studio zeal
  ];

  # Bluetooth headset buttons: <https://nixos.wiki/wiki/Bluetooth>
  services.mpris-proxy.enable = true;

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
