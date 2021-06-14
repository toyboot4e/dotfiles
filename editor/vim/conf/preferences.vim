" preferences.vim

" --------------------------------------------------------------------------------
" Configuration

let g:mapleader = ' '

let g:use_wintabs = 0
let g:nerdtree_tabs_open_on_console_startup = 0

" REMARK: The width of word wrapping in fzf-preview should be decided dynamically
let g:fzf_preview_command = 'bat --color=always --style=grid {-1} --wrap --terminal-width 84'

let g:goyo_preferred_width = 88

" --------------------------------------------------------------------------------
" System

" show last line instead of @ symbol
set display+=lastline

" show unvisible characters
set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
set list

" for vim-which-key
" (Don't make it too short! Even if it worked for now, later it won't)
set timeoutlen=500

" --------------------------------------------------------------------------------
" Preferences

" Tips: use augroup for `autocmd`s so that they don't duplicate

" ----------------------------------------
" Theme

syntax on

" Do not make vim slower when there are very long lines
set synmaxcol=200

" ----------------------------------------
" Tabs

set scrolloff=3   " margins for scrolling
" set nostartofline " keep cursor position on screen with `C-f` or `C-b`
set tabstop=4
set shiftwidth=4
set expandtab    " prefer spaces to tabs (except make files)

augroup UseTab
    autocmd FileType make set noexpandtab
augroup END

" ----------------------------------------
" Completion settings

set completeopt+=longest,menuone,noselect
let g:jedi#popup_on_dot = 1
let g:mucomplete#enable_auto_at_startup = 1

" ----------------------------------------
" Language specific settings

" augroup html_tab_width
"     autocmd FileType html setlocal expandtab shiftwidth=4
" augroup END

" C
augroup project
    autocmd!
    autocmd BufRead,BufNewFile *.h,*.c set filetype=c
augroup END

" ----------------------------------------
" Smarter `bn`, `bp` to skip terminal and quickfix buffers

augroup SmarterNavigation
    autocmd!
    autocmd TermOpen * set nobuflisted
    autocmd BufWinEnter quickfix set nobuflisted
augroup END

