{
  lib,
  useX,
  config,
  pkgs,
  ...
}:
lib.mkIf (!useX) {
  # Hyperland
  # https://wiki.hypr.land/Nix/Hyprland-on-NixOS/
  programs.hyprland.enable = true;

  # For Sway:
  # https://wiki.nixos.org/wiki/Sway
  environment.systemPackages = with pkgs; [
    grim # screenshot functionality
    slurp # screenshot functionality
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    mako # notification system developed by swaywm maintainer
    way-displays # auto-scale outputs from DPI + handle hotplug
    wlogout # graphical logout/lock/reboot/shutdown menu
  ];

  environment.etc."way-displays/cfg.yaml".text = ''
    SCALING: true
    AUTO_SCALE: true

    # Matches the `output ... scale 1.5` line in the sway config so the two never fight.
    SCALE:
      - NAME_DESC: 'BenQ PD2730S'
        SCALE: 1.5

    # Remove way-displays warning for VRR
    VRR_OFF:
      - 'BenQ PD2730S'

    # Default callback runs notify-send (not installed); silence it to avoid errors.
    CALLBACK_CMD: ""
  '';

  # For Electron and Chromium: https://nixos.wiki/wiki/Wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  services.greetd = {
    enable = true;
    settings = {
      # wow, this shortcuts login
      default_session = {
        command = "sway";
        user = "tbm";
      };
    };
  };

  services.displayManager.defaultSession = "sway";
  programs.sway = {
    enable = true;
    xwayland.enable = true;
    wrapperFeatures.gtk = true;
  };

  # For plover:
  services.udev.extraRules = ''
    KERNEL=="uinput", GROUP="input", MODE="0660", OPTIONS+="static_node=uinput"
  '';
}
