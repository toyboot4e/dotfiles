# ~/.config/fish/config.fish

# If `bass` is installed, I assume I'm using Home Manager on Nix
# (bass: https://github.com/edc/bass)
if command -sq "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
    bass source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
end

status --is-interactive; and source ~/dotfiles/shell/fish/interactive.fish


# `ghcup`
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /Users/tbm/.ghcup/bin $PATH # ghcup-env

