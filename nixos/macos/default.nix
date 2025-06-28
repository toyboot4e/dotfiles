{ pkgs, ... }:
{
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
