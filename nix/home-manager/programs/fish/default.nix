sources:
{ config, pkgs, ... }:
{
  programs.fish = {
    enable = true;
    plugins =
      map
        (p: {
          name = p.pname;
          inherit (p) src;
        })
        (
          with pkgs.fishPlugins;
          [
            bass
            foreign-env
            fzf-fish
          ]
        )
      ++ [
        {
          name = "fish-ghq";
          inherit (sources.fish-ghq) src;
        }
      ];
    shellInit = ''
      source ${config.home.homeDirectory}/dotfiles/shell/fish/config.fish
    '';
  };
}
