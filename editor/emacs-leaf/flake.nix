{
  description = "A basic flake with a shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        tangleCommand = pkgs.writeShellApplication {
          name = "tangle-command";
          runtimeInputs = [ pkgs.emacs ];
          text = ''
            emacs -Q --batch --eval "(require 'ob-tangle) (org-babel-tangle-file 'init.org')"
          '';
        };
      in
      {
        # nix run .#tangle
        apps.tangle = flake-utils.lib.mkApp {
          drv = tangleCommand;
        };
      }
    );
}
