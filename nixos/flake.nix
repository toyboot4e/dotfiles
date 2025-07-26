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

    # It also fails to install though:
    # https://github.com/nix-giant/nix-darwin-emacs
    darwin-emacs = {
      url = "github:nix-giant/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin-emacs-packages = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # https://github.com/slotThe/emacs-lsp-booster-flake
    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";

    # https://github.com/openstenoproject/plover-flake
    # plover-flake.url = "github:openstenoproject/plover-flake";
    plover-flake.url = "github:toyboot4e/plover-flake?ref=my-test";
    # plover-flake.inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # plover-flake.url = "github:toyboot4e/plover-flake?ref=macos";
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
      darwin-emacs,
      darwin-emacs-packages,
      plover-flake,
      ...
    }:
    let
      useX = true;
      # useX = false;
    in
    (
      let
        system = "x86-64_linux";
        pkgs = import nixpkgs { inherit system; };
      in
      {
        formatter.${system} = pkgs.nixfmt-tree;
        packages.${system}.default = inputs.fenix.packages.${system}.default.toolchain;
        nixosConfigurations.tbm = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit system useX;
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
                inherit inputs system useX;
              };

              home-manager.users.tbm = import ./hosts/tbm;
            }
          ];
        };
      }
    )
    // (
      let
        system = "aarch64-darwin";
        pkgs = import nixpkgs { inherit system; };
        my-emacs-overlay =
          # bug: https://github.com/NixOS/nixpkgs/issues/395169
          (
            final: prev: {
              emacs = prev.emacs.override {
                withNativeCompilation = false;
              };
              emacs-unstable = prev.emacs-unstable.override {
                withNativeCompilation = false;
              };
              emacs-git = prev.emacs-git.override {
                withNativeCompilation = false;
              };
            }
          );
      in
      {
        formatter.${system} = pkgs.nixfmt-tree;
        packages.${system}.default = inputs.fenix.packages.${system}.default.toolchain;
        darwinConfigurations.mac = nix-darwin.lib.darwinSystem {
          inherit system;
          specialArgs = {
            inherit system;
          };
          modules = [
            {
              users.users.mac.home = "/Users/mac";
              nixpkgs.overlays = [
                emacs-overlay.overlay
                emacs-lsp-booster.overlays.default
                my-emacs-overlay
                fenix.overlays.default
              ];
            }

            ./nix-darwin
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = {
                inherit inputs system;
              };

              home-manager.users.mac = import ./hosts/mac;
            }
          ];
        };
      }
    );
}
