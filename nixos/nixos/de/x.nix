{
  useX,
  lib,
  config,
  pkgs,
  ...
}:
lib.mkIf useX {
  # X11 + i3
  # https://nixos.wiki/wiki/I3
  # links /libexec from derivations to /run/current-system/sw
  # TODO: needed?
  environment.pathsToLink = [ "/libexec" ];

  services.displayManager.defaultSession = "none+i3";
  services.xserver = {
    enable = true;
    dpi = 163;
    # dpi = 326;

    # output the configuration file to `/etc/X11/xorg.conf` so that I can see it easily:
    exportConfiguration = true;

    desktopManager = { };

    # FIXME: enable Nvidia only when the device is found
    # # https://nixos.wiki/wiki/Nvidia
    # videoDrivers = ["nvidia"];
    # # less screen tearing
    # screenSection = ''
    #   Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
    #   Option         "AllowIndirectGLXProtocol" "off"
    #   Option         "TripleBuffer" "on"
    # '';

    # WM
    windowManager.i3 = {
      enable = true;

      extraPackages = with pkgs; [
        dmenu
        # TODO: Rofi here?
        # rofi
        i3status
        i3lock
        i3blocks # if you are planning on using i3blocks over i3status
      ];
    };

    # Keyboard
    # TODO: consier using `extraLayouts` instead?
    # https://nixos.wiki/wiki/Keyboard_Layout_Customization
    # xkbModel = "ja106";
    # xkbOptions = "ctrl:nocaps";
    xkb = {
      # setxkbmap -query
      # setxkbmap jp
      layout = "jp";
      model = "ja106";
      options = "ctrl:nocaps";
    };

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
}
