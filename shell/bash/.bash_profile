# Login-specific initilizations (setting up environmental variable)

# needed?
source ~/.bashrc

_path "$HOME/bin"
_path "$HOME/.local/bin"

_path "$HOME/.nix-profile/bin"
_path "$HOME/.npm-global/bin"

# # SDL2
# export LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"

# [ -r '/usr/local/etc/bash_completion.d/git-completion.bash' ] && src '/usr/local/etc/bash_completion.d/git-completion.bash'
# [ -r "$HOME/.fzf.bash" ] && src "$HOME/.fzf.bash"

# FIXME: such commands all make my bash crash, why

# # if _exists w3m ; then
# if command -v w3m > /dev/null ; then
#     export BROWSER=w3m
# fi

# # --------------------------------------------------------------------------------
# # Virtual environment (?)
# # --------------------------------------------------------------------------------
# 
# # # Go
# # if [ _exists goenv ] ; then
# #     eval "$(goenv init -)"
# #     export GOPATH="$(go env GOPATH)"
# # fi

# `rbenv`
# _exists rbenv && eval "$(rbenv init -)"

# `pipenv` or such
# _exists pipenv && eval "$(pipenv --completion)"

# # --------------------------------------------------------------------------------
# # PATH
# # --------------------------------------------------------------------------------
# 
# # Load RVM into a shell session *as a function*
# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# home-manager session variables
if [ -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ] ; then
    source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" 
fi

# # `ghcup`
# if [ -d "$HOME/.ghcup" ] ; then
#     export PATH="$PATH:$HOME/.ghcup/bin"
# fi

# # `rvm`
# if [ -d "$HOME/.rvm" ] ; then
#     export PATH="$PATH:$HOME/.rvm/bin"
# fi

# # TODO: I need to remember what is this
# if [ -d "$HOME/.sdkman" ]; then
#     export SDKMAN_DIR="$HOME/.sdkman"
#     [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
# fi

# # [ -r "$HOME/.bashrc" ] && source "$HOME/.bashrc" && psn

