" general.vim

" --------------------------------------------------------------------------------
" System

language C           " English as the system language
syntax enable        " Default syntax highlight by Vim enabled
set belloff=all      " No sound
set mouse=nv         " Mouse input enabled
set backspace=2      " Backspace enabled in insert mode
filetype plugin on   " I forgot what this is
set updatetime=100   " Make vim faster

" --------------------------------------------------------------------------------
" File

set encoding=utf-8  " Use UTF-8
set noswapfile      " Do not create unnecessary files
set nobackup        " Do not create unnecessary files
set noundofile      " Do not create unnecessary files
set hidden          " Switch buffer when it's not yet saved

set autoread                              " Reload when the file is modified
autocmd FocusGained,BufEnter * :checktime " detect external changes
autocmd FocusGained,BufEnter * :redraw!   " ..and automatically refresh screen

" --------------------------------------------------------------------------------
" Preferences

" TIPS: use `:noXXX` to disable an option

" ----------------------------------------
" View

set showtabline=2  " Show tab (even when no such plugin is enabled)
set showcmd        " Show input at right bottom coner
set ruler          " Show the line and column number of the cursor position
set showmatch      " Show matching pair of symbols
set number         " Show line numbers

" search
set incsearch      " Incremental serach (ESC or C-[ to cancel)
set ignorecase     " Ignoring up/down case
set smartcase      " Only down case ignores up/down
set hlsearch       " Enables highlighting matches

" fold
set fillchars=fold:\  " do not show `...` characters

function! ToyFoldText()
    let title = substitute(getline(v:foldstart),"^ *","",1)
    let txt = '+ ' . title . '                  '
    return txt
endfunction
set foldtext=ToyFoldText()

" show folding range left to the line numbers
" set foldtext=FoldCCtext()
" set foldcolumn=3
" set fillchars=vert:\|

" ----------------------------------------
" Coding

set foldmethod=indent
set foldlevel=99
set autoindent
set smartindent

