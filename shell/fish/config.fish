# ~/.config/fish/config.fish

status --is-interactive; and source ~/dotfiles/shell/fish/interactive.fish

if command -sq rvm
    rvm default
end

# tabtab source for electron-forge package
# uninstall by removing these lines or running `tabtab uninstall electron-forge`
[ -f /Users/toy/repos/xi-electron/node_modules/tabtab/.completions/electron-forge.fish ]; and . /Users/toy/repos/xi-electron/node_modules/tabtab/.completions/electron-forge.fish
