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
  # programs.hyprland.enable = true;

  # Sway
  # https://wiki.nixos.org/wiki/Sway
  environment.systemPackages = with pkgs; [
    grim # screenshot functionality
    slurp # screenshot functionality
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    mako # notification system developed by swaywm maintainer
  ];

  # For Electron and Chromium: https://nixos.wiki/wiki/Wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  services.displayManager.defaultSession = "sway";

  programs.sway = {
    enable = true;
    xwayland.enable = true;
    wrapperFeatures.gtk = true;
  };
}
