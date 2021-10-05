" .vimrc

" --------------------------------------------------------------------------------
" Guides

" ----------------------------------------
" Interactive commands

" press space to expand:
" - `:s`  -> :source ~/.vimrc
" - `:ed` -> :edit ~/.vimrc

" gf: go to file
" gB: open URL in browser
" gG: (not ready; open GitHub page)

" ----------------------------------------
" vim-plug

" https://github.com/junegunn/vim-plug

" ```
" $ curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
"     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
" $ pip3 install pynvim
" ```

" ----------------------------------------
" Nerd Fonts

" Be sure to get one installed and use it in your teminal:
" https://github.com/ryanoasis/nerd-fonts#font-installation

" --------------------------------------------------------------------------------
" Setup

" True color support
set termguicolors
let &t_8f = '\<Esc>[38;2;%lu;%lu;%lum'
let &t_8b = '\<Esc>[48;2;%lu;%lu;%lum'

" Let plugins use bash
let $SHELL = 'bash'

" Use fish in embedded terminals
if executable('fish')
    set shell=fish
endif

" --------------------------------------------------------------------------------
" Procedures

function s:SetupColors()
    " we first set bubble gem, and then smyck
    colorscheme bubblegum
    colorscheme smyck

    hi Folded guibg=NONE ctermbg=NONE

    " gitgutter
    highlight! link SignColumn LineNr

    " ----------------------------------------
    " GVim

    set guifont=SauceCodeProNerdFontComplete-Regular:h10
    set guicursor=a:blinkon0

endfunction

source $HOME/dotfiles/editor/vim/conf/general.vim
source $HOME/dotfiles/editor/vim/conf/preferences.vim
source $HOME/dotfiles/editor/vim/conf/japanese.vim

source $HOME/dotfiles/editor/vim/conf/plugin_config_preload.vim
source $HOME/dotfiles/editor/vim/conf/plugins.vim

call s:SetupColors()

source $HOME/dotfiles/editor/vim/conf/plugin_config.vim
source $HOME/dotfiles/editor/vim/conf/mappings.vim

" Other than C#
source $HOME/dotfiles/editor/vim/conf/coc.vim
" C#
" source $HOME/dotfiles/editor/vim/conf/omni.vim

