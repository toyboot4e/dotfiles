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
    fenix.inputs.nixpkgs.follows = "nixpkgs";

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # https://github.com/nix-community/emacs-overlay
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    # https://github.com/slotThe/emacs-lsp-booster-flake
    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";

    # https://github.com/openstenoproject/plover-flake
    plover-flake.url = "github:openstenoproject/plover-flake";
    plover-flake.inputs.nixpkgs.follows = "nixpkgs";
    plover-flake.inputs.treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    # plover-flake.url = "github:toyboot4e/plover-flake?ref=macos-test";
    # plover-flake.inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # plover-flake.url = "github:toyboot4e/plover-flake?ref=macos";

    # https://github.com/ryoppippi/nix-claude-code
    nix-claude-code.url = "github:ryoppippi/nix-claude-code";
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
      nix-claude-code,
      ...
    }:
    let
      useX = true;
      # https://isabelroses.com/blog/im-not-mad-im-disappointed/
      forAllSystems =
        f:
        nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (system: f nixpkgs.legacyPackages.${system});

      # Base system (`nix/nix-darwin`) + per-user entries (`nix/hosts/<user>`)
      mkDarwin =
        {
          user,
          firstBoot ? false,
        }:
        nix-darwin.lib.darwinSystem {
          specialArgs = {
            inherit user firstBoot;
          };
          modules = [
            {
              users.users.${user}.home = "/Users/${user}";
              nixpkgs.overlays = [
                emacs-overlay.overlay
                emacs-lsp-booster.overlays.default
                fenix.overlays.default
              ];
            }

            ./nix/nix-darwin
            ./nix/hosts/${user}/darwin.nix

            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true; # inherit the system's nixpkgs
              home-manager.useUserPackages = true;
              home-manager.backupFileExtension = "hm-backup";
              home-manager.extraSpecialArgs = {
                inherit inputs;
              };

              home-manager.users.${user} = import ./nix/hosts/${user}/home.nix;
            }
          ];
        };
    in
    {
      formatter = forAllSystems (pkgs: pkgs.nixfmt-tree);
      packages = forAllSystems (pkgs: {
        default = inputs.fenix.packages.${pkgs.stdenv.hostPlatform.system}.default.toolchain;
      });

      nixosConfigurations.tbm = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit useX;
        };
        modules = [
          {
            nixpkgs.overlays = [
              emacs-overlay.overlay
              emacs-lsp-booster.overlays.default
              fenix.overlays.default
              nix-claude-code.overlays.default
            ];

            # https://github.com/ryoppippi/nix-claude-code
            nix.settings = {
              extra-substituters = [ "https://ryoppippi.cachix.org" ];
              extra-trusted-public-keys = [
                "ryoppippi.cachix.org-1:b2LbtWNvJeL/qb1B6TYOMK+apaCps4SCbzlPRfSQIms="
              ];
            };
          }
          ./nix/nixos

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true; # inherit the system's nixpkgs
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-backup";
            home-manager.extraSpecialArgs = {
              inherit inputs useX;
            };

            home-manager.users.tbm = import ./nix/hosts/tbm;
          }
        ];
      };

      # `<user>-first-boot` leaves out input-grabbing agents until Accessibility is granted
      darwinConfigurations =
        nixpkgs.lib.concatMapAttrs
          (user: args: {
            ${user} = mkDarwin ({ inherit user; } // args);
            "${user}-first-boot" = mkDarwin (
              {
                inherit user;
                firstBoot = true;
              }
              // args
            );
          })
          {
            fr = { };
            mp = { };
          };
    };
}
