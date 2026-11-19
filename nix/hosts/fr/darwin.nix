# nix-darwin entries only for `fr`, on top of `nix/nix-darwin`
{
  homebrew = {
    # WARNING: It deletes homebrew packages not installed via Nix
    onActivation.cleanup = "uninstall";
    taps = [ ];
    brews = [
      "ghcup"
    ];
    casks = [
      "claude"
      "gimp"
    ];
  };
}
