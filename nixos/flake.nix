{
  # sudo nixos-rebuild --flake .#tbm
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # nixpkgs-stable.url = "github:NixOS/nixpkgs/nixpkgs-stable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    # my fork for local tests
    my-nixpkgs.url = "github:toyboot4e/nixpkgs?ref=online-judge-verify-helper";
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
    plover-flake.url = "github:openstenoproject/plover-flake";
    # plover-flake.url = "github:toyboot4e/plover-flake?ref=ini-json";
    # plover-flake.url = "github:toyboot4e/plover-flake?ref=stenobee";
    # WIP: works? https://github.com/openstenoproject/plover-flake/issues/232
    # plover-flake.inputs.nixpkgs.follows = “nixpkgs”;
    # plover-flake.url = "github:toyboot4e/plover-flake?ref=psutil";
    # plover-flake.url = "github:toyboot4e/plover-flake?ref=merge";
    # If we want to build non-built in Python plugins, we must match the versions of Python, so:

    # NOTE: It does not work for lapwing-aio
    # - lapwing-aio uses older version of setuptools, which is for plover v4.
    #   https://github.com/aerickt/plover-lapwing-aio/pull/11
    #   - relateve Plover issue
    #     https://github.com/openstenoproject/plover/issues/1714
    # plover-flake.inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs@{
      nixpkgs,
      nix-darwin,
      my-nixpkgs,
      home-manager,
      fenix,
      emacs-overlay,
      emacs-lsp-booster,
      plover-flake,
      ...
    }:
    let
      useX = true;
      my-pkgs = import my-nixpkgs {
          system = "x86_64-linux";
      };
      # useX = false;
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
              inherit inputs useX my-pkgs;
            };

            home-manager.users.tbm = import ./tbm;
          }
        ];
      };

      packages.aarch64-darwin.default = inputs.fenix.packages.x86_64-linux.default.toolchain;
      darwinConfigurations.mac = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = {
        };
        modules = [
          {
            users.users.mac.home = "/Users/mac";
            nixpkgs.overlays = [
              emacs-overlay.overlay
              emacs-lsp-booster.overlays.default
              fenix.overlays.default
            ];
          }
          ./nix-darwin
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              inherit inputs;
            };

            home-manager.users.mac = import ./hosts/mac;
          }
        ];
      };
    };
}
