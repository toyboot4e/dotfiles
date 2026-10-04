{
  lib,
  useX,
  config,
  pkgs,
  ...
}:
lib.mkIf useX {
  services.sxhkd.enable = true;
  # Keeps `services.sxhkd` from generating a file inside the linked directory
  xdg.configFile."sxhkd/sxhkdrc".enable = false;
}
