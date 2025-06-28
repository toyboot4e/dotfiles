{ pkgs, inputs, ...}:
{
  home.packages = with pkgs; [
    kitty
    firefox
    google-chrome

    eza
    ripgrep
 ];

  home.stateVersion = "25.05";
}
