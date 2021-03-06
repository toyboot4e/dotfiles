# input mode
set -o emacs

export HISTCONTROL=ignoredups

# --------------------------------------------------------------------------------
# PATH

# Put it here so that they are loaded if a bash script is run in another shell

# TODO: should I split bashrc and bash.profile?

_exists() { command -v "$1" > /dev/null ; }

_path() {
    [ $# -eq 0 ] && return
    [ -d "${1}" ] || return
    printf "$PATH" | tr : '\n' | grep ^"${1}"'$' > /dev/null
    [ $? -eq 0 ] && return
    export PATH="${1}:$PATH"
}

src() {
    if [ -f "$1" ] ; then
        source "$1"
    else
        echo "${1} is not a file" 1>&2
    fi
}

# defer unset..

# --------------------------------------------------------------------------------
# System

# Utilities
_exists() { command -v "$1" > /dev/null ; }

_alias() {
    if _exists "$1" ; then
        alias "$2=${3:-$1}"
    fi
}

path() { printf "${PATH}" | tr : '\n' ; }

# Prompt strings
psn() { export PS1='\[\e[0;32m\]\w \[\e[0m\]\$ ' ; }
pss() { export PS1='\$ '; }
psn

# Languages
if _exists python3 ; then
    py() {
      if [ $# -eq 0 ] ; then
        python3 main.py
      else
        python3 "$@"
      fi
    }
    alias pyrepl='python3'
fi

if _exists mypy ; then
    my() {
      if [ $# -eq 0 ] ; then
        mypy main.py
      else
        mypy "$@"
      fi
    }
fi

# --------------------------------------------------------------------------------
# Aliases

_alias source src
_alias git g
_alias cargo c
if _exists emacs ; then
    em() { emacs -nw "$@" ; }
    emd() { emacs --daemon "$@" ; }
    emc() { emacsclient -nw "$@" ; }
fi
if _exists nvim ; then
    # vim() { nvim "$@" ; }
    n() { nvim "$@" ; }
else
    n() { nvim "$@" ; }
fi

# Manual
man() { command man -P 'less -isNMR' "$@" ; }
mantable() { man "$@" | grep -v '^\s' | grep -v '^\s*$' | column ; }

# cd
..() { cd ../"$1"; }
...() { cd ../../"$1"; }
....() { cd ../../../"$1"; }
.....() { cd ../../../../"$1"; }

# --------------------------------------------------------------------------------
# System commands

# cd -> pushd
# TODO: cd with fzf
cd() { command pushd "$@" > /dev/null ; }
pd() { command popd "$@"; }
mkcd() { mkdir -p -- "$@" && cd "$@"; }

# view
ls() { command ls -FG "$@"; }
lsd() { command ls -FG "$@" | grep '/' | column; }
lsn() { command ls "$@" | sort -n | column; }
ll() { ls -lA "$@" ; }

if _exists tree ; then
  tree() { command tree -CN "$@"; }
fi

# text
pc() { command pbcopy "$@"; }
_alias less l
less() { command less -iNMR "$@"; }
grepc() { command grep --color=always "$@"; }

# bat <- cat
if _exists bat ; then
    alias b=bat
    bat() {
        command bat --style="header,changes" "$@" ;
    }
    export BAT_PAGER='less -isNMR'
fi

# exa <- ls, tree
if _exists exa ; then
  exa() { command exa -F "$@"; }
  alias e='exa'
  alias el='exa -la'
  alias ea='exa -a'
  alias ed='exa -d'
  alias eT='exa -dT'
  alias edT='exa-dT'
fi

if _exists fzf ; then
  alias f='fzf --ansi --select-1 --multi'
fi

# Browsers
_alias gg googler
_alias tealdeer tldr
_alias w3m w 'w3m -B'

# Downloaders
_alias youtube_dl dl_mp3 'youtube-dl -x --audio-format mp3 --embed-thumbnail'
_alias ffmpeg audio-convert 'ffmpeg -i'

# else
_alias asciidoctor adoc

# --------------------------------------------------------------------------------
# Preference

##### narou routines ######
if _exists narou ; then
  narou() {
    pushd `@narou` > /dev/null || return 1
    command narou "$@"
    [ "$1" = 'd' -o "$1" = 'send' ] && narou-rename
    popd > /dev/null
  }
  alias nd='narou d'
fi

# --------------------------------------------------------------------------------
# EoF

# THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/toy/.sdkman"
[[ -s "/Users/toy/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/toy/.sdkman/bin/sdkman-init.sh"

