{
  description = "A basic flake for generating `init.lua`";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        tangleCommand = pkgs.writeShellApplication {
          name = "run-tangle";
          runtimeInputs = with pkgs; [ emacs ];
          text = ''
            ./tangle;
          '';
        };
      in
      {
        apps.tangle = flake-utils.lib.mkApp {
          drv = tangleCommand;
        };
      }
    );
}
