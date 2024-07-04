{
  # sudo nixos-rebuild --flake .#tbm
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    fenix.url = "github:nix-community/fenix/monthly";
  };

  outputs = inputs@{ nixpkgs, home-manager, ... }: {
    packages.x86_64-linux.default = inputs.fenix.packages.x86_64-linux.default.toolchain;
    nixosConfigurations.tbm = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./nixos
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.tbm = import ./tbm;
        }
      ];
    };
  };
}
