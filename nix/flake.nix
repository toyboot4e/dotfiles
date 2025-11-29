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
    # plover-flake.url = "github:toyboot4e/plover-flake?ref=macos-test";
    # plover-flake.inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # plover-flake.url = "github:toyboot4e/plover-flake?ref=macos";

    edgepkgs.url = "github:natsukium/edgepkgs";
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
      edgepkgs,
      ...
    }:
    let
      useX = true; # or Wayland
      # https://isabelroses.com/blog/im-not-mad-im-disappointed/
      forAllSystems =
        f:
        nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (system: f nixpkgs.legacyPackages.${system});
    in
    {
      formatter = forAllSystems (pkgs: pkgs.nixfmt-tree);
      packages.default = forAllSystems (pkgs: inputs.fenix.packages.${pkgs.stdenv.hostPlatform.system}.default.toolchain);

      nixosConfigurations.tbm = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit forAllSystems useX;
        };
        modules = [
          {
            nixpkgs.overlays = [
              emacs-overlay.overlay
              emacs-lsp-booster.overlays.default
              fenix.overlays.default
              edgepkgs.overlays.default
            ];
          }
          ./nixos

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true; # inherit the system's nixpkgs
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              inherit inputs useX;
            };

            home-manager.users.tbm = import ./hosts/tbm;
          }
        ];
      };

      darwinConfigurations.mac = nix-darwin.lib.darwinSystem {
        specialArgs = {
          inherit forAllSystems;
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

          (import ./nix-darwin "mac")
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true; # inherit the system's nixpkgs
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              inherit inputs;
            };

            home-manager.users.mac = import ./hosts/mac;
          }
        ];
      };

      darwinConfigurations.mp = nix-darwin.lib.darwinSystem {
        specialArgs = {
          inherit forAllSystems;
        };
        modules = [
          {
            users.users.mp.home = "/Users/mp";
            nixpkgs.overlays = [
              emacs-overlay.overlay
              emacs-lsp-booster.overlays.default
              fenix.overlays.default
              edgepkgs.overlays.default
            ];
          }

          (import ./nix-darwin "mp")
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true; # inherit the system's nixpkgs
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              inherit inputs;
            };

            home-manager.users.mp = import ./hosts/mac;
          }
        ];
      };
    };
}
