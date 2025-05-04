{
  config,
  pkgs,
  ...
}: {
  networking.hostName = "tbm";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable the OpenSSH daemon.
  programs.ssh.startAgent = true;
  services.openssh = {
    enable = true;
    # On NixOS 23.05, duplicate key?:
    # settings = {
    #   passwordAuthentication = false;
    #   kbdInteractiveAuthentication = false;
    # };
    # permitRootLogin = "yes";
  };

  # # https://nixos.org/manual/nixos/stable/index.html#module-services-flatpak
  # xdg.portal = {
  #   enable = true;
  #   # config.common.default = "gtk";
  #   # FIXME: I used to use this:
  #   extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  # };

  nixpkgs.config.allowUnfree = true;

  # Enable `nix-ld`:
  programs.nix-ld.enable = true;

  # Audio
  # nixpkgs.config.pulseaudio = true;
  # hardware.pulseaudio = {
  #   enable = true;
  #   support32Bit = true;
  #   extraConfig = "load-module module-combine-sink";
  #   package = pkgs.pulseaudioFull;
  # };

  # https://opentabletdriver.net/Wiki/Install/Linux#nixos
  hardware.opentabletdriver.enable = true;

  # QMK
  hardware.keyboard.qmk.enable = true;

  # lower moise for microphones
  programs.noisetorch.enable = true;

  # `bitwig` is distributed with `flatpak` for example:
  # <https://flathub.org/apps/details/com.bitwig.BitwigStudio>
  # services.flatpak.enable = true;

  # iOS: <https://nixos.wiki/wiki/IOS>
  services.usbmuxd.enable = true;

  # mount command
  services.udisks2.enable = true;

  # automount
  services.gvfs.enable = true;

  # Bluetooth
  services.blueman.enable = true;

  # ollama: https://wiki.nixos.org/wiki/Ollama
  services.ollama = {
    enable = true;
    acceleration = "cuda";
  };

  hardware.bluetooth = {
    enable = true;
    # hsphfpd.enable = true; # HSP & HFP daemon
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };

  # # Bluetooth fix
  # # <https://github.com/NixOS/nixpkgs/issues/170573>
  # fileSystems."/var/lib/bluetooth" = {
  #   devic = "/persist/var/lib/bluetooth";
  #   options = [ "bind" "noauto" "x-systemd.automount" ];
  #   noCheck = true;
  # };

  # TODO: nedded?
  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;

  # FIXME: enable Nvidia only when the device is found
  # # https://nixos.wiki/wiki/Nvidia#Fix_graphical_corruption_on_suspend.2Fresume
  # hardware.nvidia.open = true;
  # # hardware.nvidia.powerManagement.enable = true;
  # hardware.nvidia-container-toolkit.enable = true;

  # Virtualization (virt-manager): <https://nixos.wiki/wiki/Virt-manager>
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = true;
      ovmf.enable = true;
    };
    # https://www.reddit.com/r/NixOS/comments/190qrpt/is_there_a_declarative_way_to_run_sudo_virsh/
    # $ sudo dirsh net-start default
    onBoot = "start";
  };

  # start default network for virt manager:
  # $ sudo virsh net-start default
  programs.virt-manager.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;

  # Docker: <https://nixos.wiki/wiki/Docker>
  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      # $DOCKER_HOST
      setSocketVariable = true;
    };
  };

  # i18n.inputMethod = {
  #   enabled = "fcitx5";
  #   fcitx5.addons = with pkgs; [ fcitx5-mozc fcitx5-gtk ];
  # };
  #
  # $ # To enable Mozc input, run:
  # $ fcitx5-configtool
  # $ # Find `mozc_tool`:
  # $ nix eval nixpkgs#fcitx5-mozc.outPath
  # $ # Old way:
  # $ # Input mode: romaji
  # $ /run/current-system/sw/lib/mozc/mozc_tool --mode_config_dialog

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # for high resolution
  services.spice-vdagentd.enable = true;

  # Steam: https://nixos.wiki/wiki/Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };

  programs.fish.enable = true;
}
