# Wrapper around fzf

# search files in a git root directory
function __fish_search_files_from_root
    fd -t f . (git rev-parse --show-toplevel 2> /dev/null) |
        fzf -m --preview='bat --color=always {}' $argv
end

# choose a window
function __fish_tmux -d "Switch tmux session"
    # tmux list-sessions -F "#{session_name}" |
    tmux list-windows -a -F '#{session_name} >> #{window_name}'
        fzf | read -l result; and tmux switch-client -t "$result"
end

# copiedfrom: https://gist.github.com/junegunn/f4fca918e937e6bf5bad
function __fish_browse_commits --description "Git browse commits"
    # TODO: faster preview
    # for the preview, we have to dynamically get the content
    set -l log_line_to_hash "echo {} | grep -o '[a-f0-9]\{7\}' | head -1"
    set -l view_commit "$log_line_to_hash | xargs -I % sh -c 'git show --color=always % | diff-so-fancy'"
    set -l copy_commit_hash "$log_line_to_hash | xclip"
    set -l git_checkout "$log_line_to_hash | xargs -I % sh -c 'git checkout %'"
    set -l open_cmd "open"

    if test (uname) = Linux
        set open_cmd "xdg-open"
    end

    set github_open "$log_line_to_hash | xargs -I % sh -c '$open_cmd https://github.\$(git config remote.origin.url | cut -f2 -d. | tr \':\' /)/commit/%'"

    # bold version:
    # git log --color=always --format='%C(auto)%h%d %s %C(green)%C(bold)%cr% C(blue)%an' | \
    git log --color=always --format='%C(yellow)%h %Creset%s%C(auto)%d %C(cyan)(%an)' | \
        fzf --no-sort --reverse --tiebreak=index --no-multi --ansi \
            --preview="$view_commit" \
            --header="ENTER to view, CTRL-Y to copy hash, CTRL-O to open on GitHub, CTRL-X to checkout, CTRL-C to exit" \
            # --bind "enter:execute:$view_commit" \
            --bind "ctrl-y:execute:$copy_commit_hash" \
            --bind "ctrl-x:execute:$git_checkout" \
            --bind "ctrl-o:execute:$github_open"
end

# the main function (same name as the file name without stem)
function f
    if not command -sq fzf
        return
    end

    if not isatty stdin
        fzf --preview='bat --color=always {}' $argv
        return
    end

    if test 0 -eq (count $argv)
        __fish_search_files_from_root $argv
        return
    end

    set -l cmd $argv[1]
    set -e argv[1]

    switch "$cmd"
        case c commits
            __fish_browse_commits $argv
        case t tmux
            __fish_tmux $argv
        case '*'
            echo "non-existing sub command: `$cmd`"
    end
end

#

