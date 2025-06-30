{
  # sudo nixos-rebuild --flake .#tbm
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # nixpkgs-stable.url = "github:NixOS/nixpkgs/nixpkgs-stable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
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
    # https://github.com/openstenoproject/plover-flake
    # plover-flake.url = "github:openstenoproject/plover-flake";
    plover-flake.url = "github:toyboot4e/plover-flake?ref=ini-json";
    # plover-flake.url = "github:toyboot4e/plover-flake?ref=merge";
    # If we want to build non-built in Python plugins, we must match the versions of Python, so:
    # plover-flake.inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs@{
      nixpkgs,
      nix-darwin,
      home-manager,
      fenix,
      emacs-overlay,
      emacs-lsp-booster,
      plover-flake,
      ...
    }:
    let
      useX = false;
    in
    {
      packages.x86_64-linux.default = inputs.fenix.packages.x86_64-linux.default.toolchain;
      nixosConfigurations.tbm = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit useX;
        };
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
            home-manager.extraSpecialArgs = {
              inherit inputs useX;
            };

            home-manager.users.tbm = import ./tbm;
          }
        ];
      };

      packages.aarch64-darwin.default = inputs.fenix.packages.x86_64-linux.default.toolchain;
      darwinConfigurations.mac = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          {
            users.users.mac.home = "/Users/mac";
            nixpkgs.overlays = [
              emacs-overlay.overlay
              emacs-lsp-booster.overlays.default
              fenix.overlays.default
            ];
          }
          ./macos
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              inherit inputs;
            };

            home-manager.users.mac = import ./mac;
          }
        ];
      };
    };
}
