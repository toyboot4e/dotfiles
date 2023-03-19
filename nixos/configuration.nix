# configuration.nix(5)
# nixos-help

# `unstable` channel by default (previously 22.11)

# TODOs:
# - [ ] Bluetooth support
# - [ ] Monitoring applications
# - [ ] Automatic USB mounting (or not)
# - [ ] Blender

{ config, pkgs, ... }:

# let
#   unstable = import <unstable> { config = { allowUnfree = true; }; };
# in

{
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # FIXME:
  nixpkgs.config.permittedInsecurePackages = [
    "python-2.7.18.6"
  ];

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # home-manager as NixOS module:
      # <https://nix-community.github.io/home-manager/index.html#sec-install-nixos-module>
      <home-manager/nixos>
    ];

  nixpkgs.config.allowUnfree = true;

  # <https://nixos.wiki/wiki/Environment_variables>
  environment.sessionVariables = {
    EDITOR = "nvim -u NONE";
    BROWSER = "firefox";
    DEFAULT_BROWSER = "firefox";
    TERMINAL = "kitty";

    XDG_CACHE_HOME  = "\${HOME}/.cache";
    XDG_CONFIG_HOME = "\${HOME}/.config";
    XDG_BIN_HOME    = "\${HOME}/.local/bin";
    XDG_DATA_HOME   = "\${HOME}/.local/share";

    GTK_IM_MODULE   = "fcitx";
    QT_IM_MODULE    = "fcitx";
    XMODIFIERS      = "@im=fcitx";
    GLFW_IM_MODULE  = "ibus";

    PATH = [
      "\${XDG_BIN_HOME}"
    ];
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

  # Sound
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    extraConfig = "load-module module-combine-sink";
    package = pkgs.pulseaudioFull;
  };
  nixpkgs.config.pulseaudio = true;

  # `bitwig` is distributed with `flatpak` for example:
  # <https://flathub.org/apps/details/com.bitwig.BitwigStudio>
  services.flatpak.enable = true;

  # https://nixos.org/manual/nixos/stable/index.html#module-services-flatpak
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # mount command
  services.udisks2.enable = true;

  # Bluetooth
  services.blueman.enable = true;
  hardware.bluetooth = {
    enable = true;
    # TODO: what is this? (it's from the guide)
    hsphfpd.enable = true; # HSP & HFP daemon
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };

  # # Bluetooth fix
  # # <https://github.com/NixOS/nixpkgs/issues/170573>
  # fileSystems."/var/lib/bluetooth" = {
  #   device = "/persist/var/lib/bluetooth";
  #   options = [ "bind" "noauto" "x-systemd.automount" ];
  #   noCheck = true;
  # };

  # Working?
  hardware.video.hidpi.enable = true;

  # Fonts https://nixos.wiki/wiki/Fonts
  fonts = {
    fontDir.enable = true;
    enableDefaultFonts = true;

    # Font packages TODO: Add more fonts
    fonts = with pkgs; [
      # SauceCodePro is distributed as SourceCodePro
      (nerdfonts.override { fonts = [ "SourceCodePro" ]; })
      noto-fonts noto-fonts-cjk font-awesome pango monoid roboto-mono vistafonts
    ];

    fontconfig = {
      enable = true;
      defaultFonts = {
        # TODO: use SourceCodePro?
        serif = [ "noto-fonts-cjk" ];
        sansSerif = [ "noto-fonts-cjk" ];
        monospace = [ "noto-sans-font-cjk" ];
     };
    };
  };

  # X11 + i3
  # https://nixos.wiki/wiki/I3
  environment.pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw
  services.xserver = {
    enable = true;

    # output the configuration file to `/etc/X11/xorg.conf` so that I can see it easily:
    exportConfiguration = true;

    desktopManager = { };
    displayManager.defaultSession = "none+i3";

    # https://nixos.wiki/wiki/Nvidia
    videoDrivers = [ "nvidia" ];

    # less screen tearing
    screenSection = ''
      Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
      Option         "AllowIndirectGLXProtocol" "off"
      Option         "TripleBuffer" "on"
    '';

    # WM
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

    # Keyboard
    # TODO: consier using `extraLayouts` instead?
    # https://nixos.wiki/wiki/Keyboard_Layout_Customization
    layout = "jp";
    xkbModel = "ja106";
    xkbOptions = "ctrl:nocaps";

    # Monitors
    # FIXME: not wokring. consider instead using displayManager.setupCommands or i3 commands
    xrandrHeads = [
      {
      	output = "HDMI-0";
	# TODO: add `DisplaySize` for corret DPI
        monitorConfig = ''
          Option "PreferredMode" "1920x1080"
          Option "Rotate" "left"
        '';
      }

      {
        output = "DP-1";
        primary = true;
        monitorConfig = ''
          Option "RightOf" "HDMI-0"
          Option "PreferredMode" "3840x2160"
        '';
        }
    ];
  };

  # TODO: nedded?
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable Japanese input using `fcitx` (fcitx4)
  #
  # https://ykonomi.hatenablog.com/entry/2021/04/27/022803
  #
  # ----
  # $ # Enable Mozc input:
  # $ fcitx-configtool
  # $ # Input mode: romaji
  # $ /run/current-system/sw/lib/mozc/mozc_tool --mode_config_dialog
  # ----
  # i18n.inputMethod = {
  #   enabled = "fcitx";
  #   fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
  # };

  # Enable `nix-ld`:
  programs.nix-ld.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vimHugeX xclip wget curl unzip killall mlocate
    vulkan-tools
    xorg.xdpyinfo pavucontrol sysstat yad xdotool

    kitty bash fish zsh tmux git gh ghq w3m fzf wezterm feh
    tree as-tree ripgrep fd bat delta diff-so-fancy difftastic exa as-tree tokei zoxide tealdeer
    direnv nix-direnv
    qutebrowser firefox chromium ffmpeg imagemagick dmenu rofi flameshot xdragon

    # semi-DE
    ranger cmus

    # Dock
    plank

    # Nix LSP: https://github.com/oxalica/nil
    nil
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
  home-manager.users.tbm.imports = [ ./home.nix ];

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
    settings = {
      passwordAuthentication = false;
      kbdInteractiveAuthentication = false;
    };
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

