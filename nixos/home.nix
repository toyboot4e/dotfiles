# `home-manager` configuraiton

{ config, pkgs, ... }:

let
#  unstable = import <unstable> { config = { allowUnfree = true; }; };
  fcitx5-mozc-patched = pkgs.fcitx5-mozc.overrideAttrs (oldAttrs: rec {
     preFixup = ''
       wrapQtApp $out/lib/mozc/mozc_tool
     '';
  });
in

{
  nixpkgs.config.allowUnfree = true;

  # auto mount: <https://nix-community.github.io/home-manager/options.html#opt-services.udiskie.enable>
  services.udiskie = {
    enable = true;
    automount = true;
    notify = false;
  };

  # nixpkgs.config.permittedInsecurePackages = [
  #   "python-2.7.18.6"
  # ];
  # i18n.inputMethod = {
  #   enabled = "fcitx";
  #   fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
  # };

  i18n.inputMethod = {
    enabled = "fcitx5";
    # fcitx5.addons = with pkgs; [ fcitx5-mozc fcitx5-gtk ];
    fcitx5.addons = with pkgs; [ fcitx5-mozc-patched fcitx5-gtk ];
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
    size = 24;
  };

  xdg.mimeApps = {
    enable = true;

    associations.added = {
      "application/pdf" = ["org.gnome.Evince.desktop"];
    };

    defaultApplications = {
      "application/pdf" = ["org.gnome.Evince.desktop"];

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

  # # fenix: <https://github.com/nix-community/fenix>
  nixpkgs.overlays = [
    (import "${fetchTarball "https://github.com/nix-community/fenix/archive/main.tar.gz"}/overlay.nix")
  ];

  home.packages = with pkgs; [
    # ----------------------------------------------------------------------------------------------------
    # kitty firefox qutebrowser rofi

    arandr bluetuith
    cinnamon.nemo gnome.nautilus xfce.thunar
    xdg-ninja emacs neovim
    # geekbench
    neofetch meson ninja

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

    # python3
    docker
    slack zulip vscode mpv gimp evince
    # blender
    # https://github.com/NixOS/nixpkgs/issues/241125
    ghc stack cabal-install zlib haskellPackages.implicit-hie ormolu
    drawio mdbook pandoc texlive.combined.scheme-full calibre minify
    pup jq watchexec
    jdk ditaa graphviz plantuml

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

