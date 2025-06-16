{
  pkgs,
  inputs,
  sources,
  ...
}:
let
  sources = pkgs.callPackage ./_sources/generated.nix { };
  harri-numbers = pkgs.callPackage ./plover/harri-numbers { };
in
inputs.plover-flake.packages.${pkgs.system}.plover.withPlugins (ps: [
  ps.plover-auto-reconnect-machine
  ps.plover-lapwing-aio
  ps.plover-console-ui
  # TODO: custom packages
  # (import ./nix/harri-numbers)
])
