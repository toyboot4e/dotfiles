# configuration.nix(5)
# nixos-help

{ config, pkgs, callPackage, ... }: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # TODO: Hide user preferences?
  time.timeZone = "Asia/Tokyo";

  # Allow most packages TODO: What does it mean?
  nixpkgs.config = {
    allowUnfree = true;
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable sudo TODO: Is it OK?
  security.sudo.enable = true;
  security.sudo.configFile = ''
    %wheel ALL=(ALL) ALL
  '';

  # TODO: Network settings
  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;
  networking.networkmanager.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp4s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # TODO: Select keyboard layout here
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Who don't want sound?
  sound.enable = true;

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

  # Set up `xfce` + `i3` (I failed to setup `i3` only environment; I even failed to enable Wifi)
  # https://nixos.wiki/wiki/I3
  environment.pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw
  services.xserver = {
    # Use the X window system
    enable = true;

    desktopManager = {
      # a. Gnome (can't be used alongside `i3`)
      # gnome.enable = true;

      # # b. `xfce` + WM
      # xfce = {
      #   enable = true;
      #   noDesktop = true;
      #   enableXfwm = false;
      # };
    };

    displayManager = {
      # a. Gnome: enable the diplsay manager
      # gdm.enable = true;

      # b. `xfce`
      # defaultSession = "xfce+i3";

      # c. `i3` only
      defaultSession = "none+i3";
    };
    
    windowManager = {
      i3 = {
        enable = true;

	# use `i3-gaps` just to change `i3bar` height
        # package = pkgs.i3-gaps; 

        extraPackages = with pkgs; [
	  # TODO: Rofi?
          dmenu
          i3status
          i3lock
          i3blocks #if you are planning on using i3blocks over i3status
       ];
      };
    };
  };

  # -----
  # Set up HomeManager (per-user package management in declarative style)
  # https://nixos.wiki/wiki/Home_Manager
  # -----

  environment.systemPackages = with pkgs; [
    # System
    rxvt_unicode
    python3
    python2

    # System GUI/utilities
    rofi
    flameshot

    # GUI
    qutebrowser firefox chromium
    discord
    slack
    vscode
    mpv
    gimp

    # Terminal
    vimHugeX   # system Vim with clipboard support
    kitty alacritty
    bash fish
    tmux
    git gh ghq
    emacs neovim nodejs
    fish

    # CLI Tools
    ffmpeg imagemagick
    # bandcamp-dl
    youtube-dl
    ripgrep fd bat delta exa as-tree
    tokei du-dust
    zoxide ranger
    tealdeer
    w3m

    # Default
    wget
    zip unzip
    # unrar
    xclip

    # Development
    cmake ninja llvm
    gcc go rustup
  ];

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # shell
  environment.binsh = "${pkgs.dash}/bin/dash";

  # this is only for root?
  users = {
    defaultUserShell = pkgs.fish;
  };

  # Set password with `passwd`
  users.users.tbm = {
    shell = pkgs.fish;
    isNormalUser = true;
    # wheek for `sudo`, `networkManager` for network stetings in non-Gnome env (`i3`)
    extraGroups = [ "wheel" "networkManager" ];
  };

  environment.variables = {
    PATH = "$HOME/.cargo/bin:$HOME/.local/bin:$HOME/go/bin:$HOME/config/bin:$HOME/bin";
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
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

