{ home, sources }:
{
  pkgs,
  inputs,
  ...
}:
let
  plover-flake = inputs.plover-flake;
  # TODO: Include Python dictionary data
  # harri-numbers = import ./harri-numbers.nix {
  #   inherit plover-flake pkgs sources;
  #   plover-flake-nixpkgs = inputs.plover-flake-nixpkgs;
  # };
  plover-dir =
    if pkgs.stdenvNoCC.isLinux then
      ".config/plover"
    else
      "Library/Application Support/plover";
in
{
  home.file."${plover-dir}/user.json".text = "{}";

  programs.plover = {
    enable = true;
    package = plover-flake.packages.${pkgs.stdenv.hostPlatform.system}.plover-full;

    # plover.cfg
    settings = {
      "Machine Configuration" = {
        machine_type = "Gemini PR";
        auto_start = true;
      };

      "Plugins" = {
        enabled_extensions = [
          "plover_lapwing_aio"
          "plover_auto_reconnect_machine"
          "plover_console_ui"
        ];
      };

      "System" = {
        name = "Lapwing";
      };

      # "System: Lapwing" = {
      #   dictionaries = [
      #     {
      #       enabled = true;
      #       path = "Harri_numbers.py";
      #     }
      #   ];
      # };

      # "Output Configuration".undo_levels = 100;
    };

  };
}
