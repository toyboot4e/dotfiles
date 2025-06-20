{
  config,
  pkgs,
  ...
}: {
  # auto mount: <https://nix-community.github.io/home-manager/options.html#opt-services.udiskie.enable>
  services.udiskie = {
    enable = true;
    automount = true;
    notify = false;
  };

  # Bluetooth headset buttons: <https://nixos.wiki/wiki/Bluetooth>
  services.mpris-proxy.enable = true;
}
