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
          name = "tangle";
          runtimeInputs = with pkgs; [emacs];
          text = ''
            emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "init.org")'
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
