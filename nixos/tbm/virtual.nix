{
  config,
  pkgs,
  ...
}: {
  # virutalization (virt-manager): https://nixos.wiki/wiki/Virt-manager
  # $ sudo virsh net start default
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };
}
