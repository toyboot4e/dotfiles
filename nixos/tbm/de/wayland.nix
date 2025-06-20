{
  useX,
  lib,
  config,
  pkgs,
  ...
}:
lib.mkIf (!useX) {
  # # Wayland
  # # https://wiki.hypr.land/Nix/Hyprland-on-Home-Manager/
  # programs.kitty.enable = true;
  # wayland.windowManager.hyprland.enable = true;

  # TODO: find sxhkd alternative
}
