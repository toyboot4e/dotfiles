# Login-specific initilizations (setting up environmental variable)
# and sourcing ~/.bashrc

_path "$HOME/bin"
_path "$HOME/.local/bin"
# _path "/Applications/MacVim.app/Contents/bin"

" SDL2
export LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"

# _exists stack && _path "$(stack path --local-bin)"

_exists rbenv && eval "$(rbenv init -)"
_exists pipenv && eval "$(pipenv --completion)"
[ -r '/usr/local/etc/bash_completion.d/git-completion.bash' ] && src '/usr/local/etc/bash_completion.d/git-completion.bash'
[ -r "$HOME/.fzf.bash" ] && src "$HOME/.fzf.bash"

_exists w3m && export BROWSER=w3m

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Go
eval "$(goenv init -)"
export GOPATH="$(go env GOPATH)"

# perl5
source /Users/toy/perl5/perlbrew/etc/bashrc

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

[ -r "$HOME/.bashrc" ] && source "$HOME/.bashrc" && psn

