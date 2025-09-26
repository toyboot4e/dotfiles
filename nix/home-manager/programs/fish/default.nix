{ sources, pkgs }:
let
  pkgNames = [
    # most fish plugins are not on nixkgs, so:
    "fish-bd" # bd command
    "fish-ghq" # ctrl + g
    "fish-nix-completions"
    "fish-nix-env"
    "fish-z" # z command
  ];
in
{
  programs.fish = {
    enable = true;
    # FIXME: broken
    # plugins = map
    #    (name:
    #    { name = sources.${name}.pname;
    #      src = sources.${name}.src;
    #    })
    #    pkgNames;
  };

  xdg.configFile."fish/config.fish".source = ../../../../shell/fish/config.fish;
}
