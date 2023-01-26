# configuration.nix(5)
# nixos-help

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # home-manager as NixOS module:
      # <https://nix-community.github.io/home-manager/index.html#sec-install-nixos-module>
      <home-manager/nixos>
    ];

  nixpkgs.config.allowUnfree = true;

  environment.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = "firefox";
    TERMINAL = "kitty";
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Asia/Tokyo";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  console = {
    # font = "Lat2-Terminus16";
    # keyMap = "us";
    # useXkbConfig = true; # use xkbOptions in tty.
    keyMap = "jp106";
    earlySetup = true;
    packages = with pkgs; [ terminus_font ];
    font = "ter-u14n";
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Fonts https://nixos.wiki/wiki/Fonts
  fonts = {
    fontDir.enable = true;
    enableDefaultFonts = true;

    # Font packages TODO: Add more fonts
    fonts = with pkgs; [
      # SauceCodePro is distributed as SourceCodePro
      (nerdfonts.override { fonts = [ "SourceCodePro" ]; })
      noto-fonts-cjk
      font-awesome
      pango # popular font
    ];

    fontconfig = {
      defaultFonts = {
        # TODO: Make sure to select SauceCodePro NerdFont and Noto Fonts
        serif = [ "noto-fonts-cjk" "SourceCodePro" ];
        sansSerif = [ "noto-fonts-cjk" "SourceCodePro" ];
        monospace = [ "noto-fonts-cjk" "SourceCodePro" ];
     };
    };
  };

  # X11 + i3
  # https://nixos.wiki/wiki/I3
  environment.pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw
  services.xserver = {
    enable = true;

    desktopManager = { };
    displayManager.defaultSession = "none+i3";

    videoDrivers = [ "nvidia" ];

    # bye, capslock
    xkbOptions = "ctrl:nocaps";

    windowManager.i3 = {
      enable = true;

      extraPackages = with pkgs; [
        dmenu
        # TODO: Rofi here?
        # rofi
        i3status
        i3lock
        i3blocks #if you are planning on using i3blocks over i3status
      ];
    };
  };

  # TODO: nedded?
  hardware.opengl.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable Japanese input
  #
  # https://ykonomi.hatenablog.com/entry/2021/04/27/022803
  #
  # ----
  # $ # Enable Mozc input:
  # $ fcitx-configtool
  # $ # Input mode: romaji
  # $ /run/current-system/sw/lib/mozc/mozc_tool --mode_config_dialog
  # ----
  i18n.inputMethod = {
    enabled = "fcitx";
    fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vimHugeX xclip wget curl
    pavucontrol sysstat yad xdotool
    kitty bash fish zsh tmux git gh ghq w3m fzf
    ripgrep fd bat delta exa as-tree tokei zoxide ranger tealdeer
    direnv nix-direnv stack cabal-install
    qutebrowser firefox ffmpeg imagemagick dmenu rofi flameshot xdragon
  ];

  # Steam: https://nixos.wiki/wiki/Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };

  # home-manager as a NixOS module: https://nix-community.github.io/home-manager/index.html#sec-install-nixos-module

  users.users.tbm = {
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "networkManager" ];
  };

  # home-manager as NixOS module: <https://nix-community.github.io/home-manager/index.html>
  home-manager.users.tbm = { pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;

    home.packages = with pkgs; [
      emacs neovim
      rustup go python3 nodejs
      discord slack vscode mpv gimp
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
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  programs.ssh.startAgent = true;
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    kbdInteractiveAuthentication = false;
    # permitRootLogin = "yes";
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

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

