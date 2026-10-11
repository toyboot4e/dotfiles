# nix-darwin entries only for `mac`, on top of `nix/nix-darwin`
{
  homebrew = {
    # WARNING: It deletes homebrew packages not installed via Nix
    onActivation.cleanup = "uninstall";
    taps = [ ];
    brews = [
      "ghcup"
    ];
    casks = [
      "discord"
      "gimp"
    ];
  };
}
