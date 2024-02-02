# ~/.config/fish/config.fish

# # If `bass` is installed, I assume I'm using Home Manager on Nix
# # (bass: https://github.com/edc/bass)
# if command -sq "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
#     bass source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
# end

status --is-interactive; and source ~/dotfiles/shell/fish/interactive.fish

# `ghcup`
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $HOME/.ghcup/bin $PATH # ghcup-env

if command -sq direnv
    direnv hook fish | source
end

# `fenv`: <https://github.com/oh-my-fish/plugin-foreign-env>
function fenv -d "Run bash scripts and import variables modified by them"
  if count $argv >/dev/null
    if string trim -- $argv | string length -q
      fenv.main $argv
      return $status
    end
  return 0
  else
    echo (set_color red)'error:' (set_color normal)'parameter missing'
    echo (set_color cyan)'usage:' (set_color normal)'fenv <bash command>'
    return 23  # EINVAL
  end
end

function fenv.main
  bash -c "$argv && env -0 >&31" 31>| while read -l -z env_var
    set -l kv (string split -m 1 = $env_var); or continue
    # Skip read-only variables
    contains $kv[1] _ SHLVL PWD; and continue
    string match -rq '^BASH_.*%%$' $kv[1]; and continue
    # Variable
    # - is not defined
    # - OR variable differs
    # - OR variable is not exported
    if not set -q $kv[1]; or test "$$kv[1]" != $kv[2]; or not set -qx $kv[1]
      set -gx $kv
    end
  end
  return $pipestatus[1]
end

# `home-manager` session variables
# <https://rycee.gitlab.io/home-manager/index.html#_why_are_the_session_variables_not_set>

if test -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
    fenv source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" > /dev/null
end

# volta
set -gx VOLTA_HOME "$HOME/.volta"
set -gx PATH "$VOLTA_HOME/bin" $PATH

