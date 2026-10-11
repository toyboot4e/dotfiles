# home-manager entries only for `mp`, on top of `nix/hosts/darwin/home.nix`
{ pkgs, ... }:
{
  imports = [ ../darwin/home.nix ];

  home.packages = with pkgs; [
  ];
}
