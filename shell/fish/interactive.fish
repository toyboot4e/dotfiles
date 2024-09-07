# referenced from: ~/.config/fish/config.fish

# TIP: do not use aliaes where it contains sub commands (after `env` or `command`)
#      because there'll be recursive completion
# TIP: run `user_path.fish` just once to set up the environmental variables universally

# $ # enable tab completion with fzf
# $ fisher add jethrokuan/fzf # https://github.com/jethrokuan/fzf
# $ # https://github.com/jethrokuan/fzf#commands
# $ set -U FZF_OPEN_COMMAND cd
# $ set -U FZF_COMPLETE 2     # https://github.com/jethrokuan/fzf/wiki/FZF-Tab-Completions
# $ # C-g for searchingghq repositories
# $ fisher install decors/fish-ghq

# TODO: only if zoxide exists
if command -sq zoxide
    zoxide init fish | source
end

alias reload "source $HOME/.config/fish/config.fish"

# my packages
source "$HOME/dotfiles/shell/fish/root_comp.fish"

# this is required to let tmux use fish as a default shell
set -x SHELL (which fish)

# --------------------------------------------------------------------------------
# OS-dependent

if test (uname) = Darwin
    function refresh_audio
        sudo launchctl stop com.apple.audio.coreaudiod && sudo launchctl start com.apple.audio.coreaudiod
    end
    function refresh_activity_monitor
        rm (fd com.apple.ActivityMonitor.plist ~)
    end
end

# --------------------------------------------------------------------------------
# Setup

# ----------------------------------------
# Constants

set NAROU_ROOT ~/Resources/narou
set NAROO_ROOT ~/Resources/narou

# ----------------------------------------
# PATHS

# run `~/dotfiles/shell/fish/user_path.fish` once to set universal paths

# prefer rvm to rbenv
# source (rbenv init - | psub)
# source (pyenv init - | psub)

# Doesn't work in Sublime?
if command -sq goenv
    source (goenv init - | psub)
    set -x GOPATH (go env GOPATH)
    set -x PATH $GOPATH/bin $PATH
end

# ----------------------------------------
# META UTILS

function _alias
    if ! command -sq $argv[1]
        return
    end

    # NOTE: never remove the quotes for the test
    if test -n "$argv[3]" # non-zero
        alias $argv[2] $argv[3]
    else
        alias $argv[2] $argv[1]
    end
end

# ----------------------------------------
# VARIABLES

set HISTCONTROL ignoredups

set -x RUST_BACKTRACE 1
# set -x RUST_LOG DEBUG

function fish_title
    set process (printf "$_")
    set cwd (pwd | sed "s@^$HOME@~@g")
    echo "$process $cwd"
end

if command -sq kitty
    kitty + complete setup fish | source
end

# NOT COMPLETELY WORKING?
function update_all
    tldr --update >/dev/null &
    npm update --g &
    gem update (gem outdated | cut -d ' ' -f 1) &
    # pip3 list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip3 install -U &
    rustup update &
    brew outdated | xargs brew update
end

_alias brew bs 'brew services'

# --------------------------------------------------------------------------------
# COMMANDS/ALIASES

# ----------------------------------------
# Application

# Now it's moved to `~/bin` so that it works in any shell
# if test -x /Applications/qutebrowser.app/Contents/MacOS/qutebrowser
#     alias qute /Applications/qutebrowser.app/Contents/MacOS/qutebrowser
# end

# ----------------------------------------
# I/O

_alias pbcopy pc

if command -sq bat
    _alias bat b
    _alias bat bn 'bat --style=plain'
    function bat
        command bat --pager="less -iMR" $argv
    end
end

_alias rg rgl 'rg -p "argv" | less -iNMR'

# Linux only
# if command -sq xclip
#     alias pbcopy='xclip -selection c'
#     alias pc='xclip -selection c'
#     alias pbpaste='xclip -selection c -o'
#     alias pp='xclip -selection c -o'
# end

# ----------------------------------------
# FILE OPERATION

if command -sq rename
    # better --dry-run output for `rename`
    function rename_
        rename --dry-run $argv 2>&1         |
        gsed 's/ would be renamed to /\t/g' |
        gsed -e '1i before'\t'after'        |
        column -ts \t
        # Note: In fish, you write espace characters if you don't quote them
        # (In bash, you write expace characters with $'\t')
    end
end

# ----------------------------------------
# TOOLS

_alias tealdeer tldr

_alias batman man

# _alias hub h
_alias git g

_alias googler gg
_alias gnuplot gp

_alias ranger r

# enables Japanese in tmux
_alias tmux tmux 'tmux -u'

_alias w3m w 'w3m -B'

# ----------------------------------------
# EDITOR

if command -sq nvim
    alias n nvim
    # alias nu 'nvim -u NONE'
    alias vim nvim
end

if command -sq vim
    alias v vim
    alias vu 'vim -u NONE'
end

# ----------------------------------------
# PROGRAMMING

# if command -sq cargo
#     alias c cargo
#     alias cn 'cargo +nightly'
#     alias rust 'cargo script'
#     alias atcoder 'cargo atcoder'
#     alias miri 'cargo +nightly miri'
# end

if command -sq cabal
    alias c cabal
end

if command -sq colordiff
    function d
        # recursive, unified context (+6 lines), 
        colordiff -rub $argv
    end
end
_alias odin od

# contextual calling
if command -sq mypy
    function my
        if test (count $argv) -eq 0
            mypy main.py
        else
            mypy $argv
        end
    end
end

if command -sq python3
    function py
        if test (count $argv) -eq 0
            python3 main.py
        else
            python3 $argv
        end
    end
end

_alias python3 1py 'python3 -c'
_alias ruby 1rb 'ruby -e'

# ----------------------------------------
# NAVIGATION

function mkcd
    if mkdir -p "$argv"
        cd "$argv"
    end
end

alias pd prevd
alias nd nextd
alias hd dirh

if command -sq eza
    alias e 'eza -F'
    alias ea 'eza -aF'
    alias el 'eza -alF'
    alias et 'eza -T'
end

if command -sq ls
    alias less 'less -iNMR'

    alias l less
    alias ls 'ls -FG'
    alias ll 'ls -AlhFG'
end

# echo with `z`
_alias z ze 'z -e'

# see `functions/f.fish` for more commands
if command -sq fzf
    set -x FZF_DEFAULT_COMMAND 'fd --type f'
end

# ----------------------------------------
# CONTENT

_alias asciidoctor adoc
_alias asciidoctor-pdf adoc-pdf

if command -sq narou
    function narou
        set original (pwd)
        cd "$NAROU_ROOT"
        command narou $argv
        # python3 ./rename.py
        cd $original
    end
end

if command -sq naroo
    function naroo
        set original (pwd)
        cd "$NAROO_ROOT"
        command naroo $argv
        cd $original
    end
end

# download mp3 with thumbnail from youtube
_alias youtube-dl dl-youtube 'youtube-dl -x --audio-format mp3 --embed-thumbnail'

# downloads content dispatching a function based on URL
function dl
    set _url $argv[1];
    switch $_url
        case "*youtube*"
            youtube-dl $argv
        case "*niconico*"
            youtube-dl $argv
        case "*bandcamp*"
            bandcamp-dl --embed-art --embed-lyrics $argv
        case "*syosetu*"
            narou d $argv
        case "*kakuyomu**"
            narou d $argv
    end
end

# args: file name album num_track max_num_track
function tag-music
    set file "$argv[1]"

    mid3v2 "$file" --artist="$argv[1]"

    if set -q "$argv[2]"
        mid3v2 "$file" --album="$argv[2]"
    end
    # track
    if set -q "$argv[3]" ; and set -q "$argv[4]"
        mid3v2 "$file" --track="$argv[3]/$argv[4]"
    end
end

# ----------------------------------------
# Converters

# reads the content of a docx file, which is a zipped file
function word
    unzip -p $argv word/document.xml
end

function word2txt
    unzip -p $argv word/document.xml |
    gsed -e 's/<w:p\ [^>]*>/\n/g'    |
    gsed -e 's/<[^>]\{1,\}>//g; s/[^[:print:]]\{1,\}//g'
end

_alias xml-to-json-fast xml2json 'cat | xml-to-json-fast'
_alias xml-to-json-fast xml2yaml 'yq r (xml2json)'

# ----------------------------------------
# FORMATTERS

function deansi
    gsed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g'
end

# xml (colorized & formatted)
if command -sq bat && command -sq xmllint
    function xml --description 'formats and colorizes a xml file'
        cat $argv | xmllint --format - | bat # pygmentize
    end
end

# yaml (colorized & formatted)
function yaml
    bat
end

