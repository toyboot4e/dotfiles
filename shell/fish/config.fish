# ~/.config/fish/config.fish

# # If `bass` is installed, I assume I'm using Home Manager on Nix
# # (bass: https://github.com/edc/bass)
# if command -sq "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
#     bass source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
# end

status --is-interactive; and source ~/dotfiles/shell/fish/interactive.fish

# `ghcup`
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /Users/tbm/.ghcup/bin $PATH # ghcup-env

if command -sq direnv
    direnv hook fish | source
end

# `home-manager` session variables
# <https://rycee.gitlab.io/home-manager/index.html#_why_are_the_session_variables_not_set>

if test -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
    # source `export=..` statements in the sh file:
    for kv in (cat "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" | grep '^export' | sed 's;^export ;;g')
        set k (printf '%s' "$kv" | cut -d '=' -f1)
        set v (printf '%s' "$kv" | cut -d '=' -f2- | tr -d '"')
	set "$k" "$v"
    end
end

