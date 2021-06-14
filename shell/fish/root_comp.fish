# root_comp.fish
#
# Expands `:/path` -> `<git root directory>/path` with <TAB>
# Formats the command line as a side effect.
#
# Source me from your `fish.config`

set -l cmds 'cd ls find grep cat'
set -a cmds 'z exa fd rg fzf bat'
set -a cmds 'vim nvim emacs'

set cmds (printf "$cmds" | tr ' ' '\n')

function __fish_do_complete_root
    set -l token (commandline -t)
    if printf '%s' "$token" | grep '^:/' > /dev/null
            and git rev-parse --show-toplevel > /dev/null ;
        return 0
    else
        return 1
    end
end

function __fish_complete_root
    # get the token where the cursor is on
    set -l token (commandline -t)

    # and get the position of it
    set -l pos (count (string match -v -- '-*' (commandline -poc)))
    set pos (math $pos + 1)

    # detect the root directory and set the replacement for the token
    set -l root (git rev-parse --show-toplevel)
    set -l repl (printf '%s' "$token" | sed 's@^:@'"$root"'@g')

    # convert the command line replacing the token
    set -l line ""
    set -l i 0
    set -l cursor_pos 0
    for token in (commandline -op)
        set i (math $i + 1)
        if test $i -ne $pos
            set line "$line" "$token"
        else
            set line "$line" "$repl"
            set cursor_pos (string length "$line")
            set cursor_pos (math $cursor_pos - 1)
        end
    end
    set line (printf '%s' "$line" | sed 's/^ //g')

    # now, overwrite the command line and set the cursor position
    commandline -r "$line"
    commandline -C $cursor_pos
end

for cmd in $cmds
    complete -c "$cmd" -f -n '__fish_do_complete_root' -a '(__fish_complete_root)'
end

