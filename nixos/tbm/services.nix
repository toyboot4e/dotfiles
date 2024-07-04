{ config, pkgs, ... }:

{
  # auto mount: <https://nix-community.github.io/home-manager/options.html#opt-services.udiskie.enable>
  services.udiskie = {
    enable = true;
    automount = true;
    notify = false;
  };

  # Use `sxhkd` service, but without overwriting the configuration file
  # - sxhkd: <https://nix-community.github.io/home-manager/options.html#opt-services.sxhkd.enable>
  # - configFile.<name>.enable: <https://rycee.gitlab.io/home-manager/options.html#opt-xdg.configFile._name_.enable>
  services.sxhkd.enable = true;
  xdg.configFile."sxhkd/sxhkdrc".enable = false;

  # Bluetooth headset buttons: <https://nixos.wiki/wiki/Bluetooth>
  services.mpris-proxy.enable = true;
}
