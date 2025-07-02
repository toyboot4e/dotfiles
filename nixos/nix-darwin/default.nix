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
      sandbox = true;
    };

    optimise.automatic = true;

    gc = {
      automatic = true;
      interval = [{ Weekday = 7; }];
      options = "--delete-older-than 7d";
    };
  };

  environment.shells = [pkgs.bash pkgs.fish];

  # TODO: really need this?
  environment.systemPackages = with pkgs; [
    yabai
    skhd
  ];
  environment.variables.EDITOR = "nvim";

  # set fish shell
  # https://github.com/nix-darwin/nix-darwin/issues/1237
  # TODO: can we use home-manager?
  programs.fish.enable = true;

  users.knownUsers = [ "mac" ];
  users.users.mac.shell = pkgs.fish;
  users.users.mac.uid = 501;

  # https://github.com/nix-darwin/nix-darwin/issues/1041
  # services.karabiner-elements = {
  #   enable = true;
  #   package = pkgs.karabiner-elements.overrideAttrs (old: {
  #     version = "14.13.0";
  #
  #     src = pkgs.fetchurl {
  #       inherit (old.src) url;
  #       hash = "sha256-gmJwoht/Tfm5qMecmq1N6PSAIfWOqsvuHU8VDJY8bLw=";
  #     };
  #
  #     dontFixup = true;
  #   });
  # };

  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = true;
    extraConfig = ''
      # sudo yabai --load-sa
      # yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
      sudo yabai
      yabai -m signal --add event=dock_did_restart action="sudo yabai"
    '';
  };

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
