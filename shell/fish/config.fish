# ~/.config/fish/config.fish

status --is-interactive; and source ~/dotfiles/shell/fish/interactive.fish

# `ghcup`
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $HOME/.ghcup/bin $PATH # ghcup-env

if command -sq direnv
    direnv hook fish | source
end

# `home-manager` session variables
# <https://rycee.gitlab.io/home-manager/index.html#_why_are_the_session_variables_not_set>

# nix profile
if test -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
    fenv source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" > /dev/null
end

# Homebrew
if test -f /opt/homebrew/bin/brew
  /opt/homebrew/bin/brew shellenv fish | source
end

# bun
# set --export BUN_INSTALL "$HOME/.bun"
# set --export PATH $BUN_INSTALL/bin $PATH

# vite-plus
# source "$HOME/.vite-plus/env.fish"

# mise (installed by `curl https://mise.run | sh`)
fish_add_path --global $HOME/.local/bin
if command -sq mise
    mise activate fish | source
end
