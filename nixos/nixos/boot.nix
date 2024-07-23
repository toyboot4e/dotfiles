{
  config,
  pkgs,
  ...
}: {
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # FIXME: GPU passthrough:
  # https://astrid.tech/2022/09/22/0/nixos-gpu-vfio/
  boot = {
    kernelParams = [
      # "iommu=pt"
      "intel_iommu=on"

      # what?: https://github.com/whimbree/nixos-configuration/blob/main/megakill/modules/vfio.nix
      # "intel_iommu=igfx_off"
      # "vfio-pci.ids=10de:2488,10de:228b"
    ];

    # FIXME: fails to boot
    # video, audio
    # https://gist.github.com/techhazard/1be07805081a4d7a51c527e452b87b26
    # extraModprobeConfig ="options vfio-pci ids=10de:2488,10de:228b";

    kernelModules = [
      "vfio"
      "vfio_iommu_type1"
      "vfio_pci"
      "vfio_virqfd"

      # "nvidia"
      # "nvidia_modeset"
      # "nvidia_uvm"
      # "nvidia_drm"
    ];
  };

  # TODO: working?
  # swapDevices = [ { device = "/dev/disk/by-label/swap"; } ];

  fileSystems."/d" = {
    device = "/dev/disk/by-uuid/85f41130-4c61-4cb9-b116-b13701009b43";
    fsType = "ext4";
    # TODO: user permission?
    options = ["users" "nofail" "exec"];
  };
}
