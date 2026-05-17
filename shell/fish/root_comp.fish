# root_comp.fish
#
# Expands `:/path` -> `<git root directory>/path` with <TAB>
#
# Source me from your `fish.config`

function __fish_expand_git_root
    set -l token (commandline -t)
    if string match -q ':/*' -- $token
        set -l root (git rev-parse --show-toplevel 2>/dev/null)
        and commandline -t -- (string replace ':' $root $token)
        return
    end
    return 1
end

function fish_complete_git_root
    if __fish_expand_git_root
        return
    end
    set -l candidates (complete -C (commandline -cp))
    if test (count $candidates) -le 1
        commandline -f complete
    else
        fzf-completion
    end
end
