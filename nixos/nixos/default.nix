# TODOs:
# - [ ] Monitoring applications
{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ./boot.nix
    ./env.nix
    ./modules.nix
    ./theme.nix
    ./x.nix
  ];

  networking.hostName = "tbm";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vimHugeX
    xclip
    wget
    curl
    unzip
    p7zip
    killall
    mlocate
    vulkan-tools

    xorg.xdpyinfo
    xorg.xev
    pavucontrol
    sysstat
    yad
    xdotool

    kitty
    bash
    fish
    # zsh
    tmux
    # zellij
    git
    gh
    ghq
    w3m
    fzf
    # wezterm
    feh
    tree
    as-tree
    ripgrep
    fd
    bat
    delta
    diff-so-fancy
    difftastic
    eza
    as-tree
    tokei
    zoxide
    tealdeer
    direnv
    nix-direnv
    # qutebrowser
    firefox
    chromium
    ffmpeg
    dmenu
    rofi
    flameshot
    dragon-drop
    # (imagemagick.override { libwebpSupport = true ; })
    imagemagick

    # semi-DE
    ranger
    cmus

    # Dock
    # plank

    # Nix
    # nld # Yet another: https://github.com/nix-community/nixd
    nil # Nix LSP: https://github.com/oxalica/nil
    nixfmt-rfc-style
    # alejandra
    nvfetcher
    rippkgs
    # nix-search-cli
    # nix-doc
    # pciutils

    # wine: <https://nixos.wiki/wiki/Wine>
    # wineWowPackages.staging winetricks

    # virt manager: <https://discourse.nixos.org/t/virt-manager-cannot-find-virtiofsd/26752>
    virtiofsd

    # On creating a shared directory, add the following inside the `<filesystem>` tag:
    # <binary path="/run/current-system/sw/bin/virtiofsd"/>

    # iOS: <https://nixos.wiki/wiki/IOS>
    libimobiledevice
    ifuse
  ];

  programs.fish.enable = true;

  users.users.tbm = {
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = ["audio" "dialout" "docker" "libvirtd" "networkManager" "wheel" "storage" "disk"];
  };

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
