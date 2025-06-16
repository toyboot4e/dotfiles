# TODO: Use extraSpecialArgs for `sources`?
sources:
# This is a NixOS module included in `imports` list, where the sum of NixOs module become our final
# output..
{
  pkgs,
  inputs,
  ...
}:
let
  plover-flake = inputs.plover-flake;
  # FIXME: Python version mismatch
  harri-numbers = import ./harri-numbers.nix {
    inherit plover-flake pkgs sources;
    plover-flake-nixpkgs = inputs.plover-flake-nixpkgs;
  };
in
{
  programs.plover = {
    enable = true;
    package = plover-flake.packages.${pkgs.system}.plover.withPlugins (ps: [
      ps.plover-auto-reconnect-machine
      ps.plover-lapwing-aio
      ps.plover-console-ui
      ps.plover-python-dictionary
      # FIXME: not working right now
      harri-numbers
    ]);
    # plover.cfg contents
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
