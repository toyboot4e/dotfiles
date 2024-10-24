{
  # sudo nixos-rebuild --flake .#tbm
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    fenix.url = "github:nix-community/fenix/monthly";
    # https://github.com/nix-community/emacs-overlay
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      # inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };
    # https://github.com/slotThe/emacs-lsp-booster-flake
    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
  };

  outputs = inputs @ {
    nixpkgs,
    home-manager,
    fenix,
    emacs-overlay,
    emacs-lsp-booster,
    ...
  }: {
    packages.x86_64-linux.default = inputs.fenix.packages.x86_64-linux.default.toolchain;
    nixosConfigurations.tbm = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        {
          nixpkgs.overlays = [
            emacs-overlay.overlay
            emacs-lsp-booster.overlays.default
            fenix.overlays.default
          ];
        }
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
