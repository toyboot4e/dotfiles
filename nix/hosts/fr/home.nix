# home-manager entries only for `mac`, on top of `nix/hosts/darwin/home.nix`
{ pkgs, ... }:
{
  imports = [
    ../darwin/home.nix
    ../../home-manager/packages-rich.nix
  ];

  home.packages = with pkgs; [
  ];
}
