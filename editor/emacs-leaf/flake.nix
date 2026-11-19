{
  description = "A basic flake with a shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    let
      forAllSystems = f: nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (system: f nixpkgs.legacyPackages.${system});
    in
    {
      # nix run (or nix run .#tangle)
      apps = forAllSystems (
        pkgs:
        let
          tangleCommand = pkgs.writeShellApplication {
            name = "tangle-command";
            runtimeInputs = [ pkgs.emacs ];
            text = ''
              emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "init.org")'
            '';
          };
          tangle = {
            type = "app";
            program = pkgs.lib.getExe tangleCommand;
            meta.description = "Tangle init.org into init.el";
          };
        in
        {
          inherit tangle;
          default = tangle;
        }
      );
    };
}
