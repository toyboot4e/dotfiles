{
  lib,
  useX,
  config,
  pkgs,
  ...
}:
lib.mkIf useX {
  # Use `sxhkd` service, but without overwriting the configuration file
  # - sxhkd: <https://nix-community.github.io/home-manager/options.html#opt-services.sxhkd.enable>
  # - configFile.<name>.enable: <https://rycee.gitlab.io/home-manager/options.html#opt-xdg.configFile._name_.enable>
  services.sxhkd.enable = true;
  xdg.configFile."sxhkd/sxhkdrc".enable = false;
}
