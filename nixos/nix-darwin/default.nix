{ pkgs, ... }:
let 
  sources = pkgs.callPackage ../_sources/generated.nix { };
in {
  # networking.hostName = "mac";
  nixpkgs.config.allowUnfree = true;
  system.primaryUser = "mac";

  nix = {
    settings = {
      experimental-features = ["nix-command" "flakes"];
    };

    optimise.automatic = true;

    gc = {
      automatic = true;
      interval = [{ Weekday = 7; }];
      options = "--delete-older-than 7d";
    };
  };

  environment.shells = [pkgs.bash pkgs.fish];

  # set fish shell
  # https://github.com/nix-darwin/nix-darwin/issues/1237
  programs.fish = {
    enable = true;
    # TODO: need nvfetcher and fetchFromGitHub
    # plugins = [
    #   {
    #     name = sources.fish-ghq.pname;
    #     src = sources.fish-ghq.src;
    #   }
    #   {
    #     name = sources.fish-nix-completions.pname;
    #     src = sources.fish-nix-completions.src;
    #   }
    #   {
    #     name = sources.fish-nix-env.pname;
    #     src = sources.fish-nix-env.src;
    #   }
    # ];
  };
  users.knownUsers = [ "mac" ];
  users.users.mac.shell = pkgs.fish;
  users.users.mac.uid = 501;

  # https://github.com/nix-darwin/nix-darwin/issues/1041
  services.karabiner-elements = {
    enable = true;
    package = pkgs.karabiner-elements.overrideAttrs (old: {
      version = "14.13.0";

      src = pkgs.fetchurl {
        inherit (old.src) url;
        hash = "sha256-gmJwoht/Tfm5qMecmq1N6PSAIfWOqsvuHU8VDJY8bLw=";
      };

      dontFixup = true;
    });
  };

  services.yabai.enable = true;
  services.skhd.enable = true;

  system = {
    defaults = {
      NSGlobalDomain.AppleShowAllExtensions = true;
      finder = {
        AppleShowAllFiles = true;
        AppleShowAllExtensions = true;
      };
      dock = {
        autohide = false;
        show-recents = false;
        orientation = "bottom";
      };
    };
  };

  system.stateVersion = 6;
}
