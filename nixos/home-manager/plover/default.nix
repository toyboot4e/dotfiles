sources:
{
  pkgs,
  inputs,
  ...
}:
let
  plover-flake = inputs.plover-flake;
in
{
  programs.plover = {
    enable = true;
    package = plover-flake.packages.${pkgs.system}.plover.withPlugins (ps: [
      ps.plover-auto-reconnect-machine
      ps.plover-lapwing-aio
      ps.plover-console-ui
      ps.plover-python-dictionary

      # harri-numbers
    ]);

    # plover.cfg
    settings = {
      "Machine Configuration" = {
        machine_type = "Gemini PR";
        auto_start = true;
      };
      "System: Lapwing" = {};
      # "Output Configuration".undo_levels = 100;
    };
  };
}
