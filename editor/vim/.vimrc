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

" --------------------------------------------------------------------------------------------------
" Abbreviations
" --------------------------------------------------------------------------------------------------

function! Abbreviate(from, to)
  exec 'cnoreabbrev <expr> '.a:from
        \ .' ((getcmdtype() ==# ":" && getcmdline() ==# "'.a:from.'")'
        \ .'? ("'.a:to.'") : ("'.a:from.'"))'
endfunction

call Abbreviate('h', 'tab help')

call Abbreviate('s', 'source ~/.vimrc')
call Abbreviate('ed', 'edit ~/.vimrc')

call Abbreviate('p', 'PlugInstall')
call Abbreviate('c', 'CocConfig')
call Abbreviate('f', 'FZF ~/')

call Abbreviate('hs', 'split')

" --------------------------------------------------------------------------------------------------
" Key mappings
" --------------------------------------------------------------------------------------------------

language C           " English as the system language
syntax enable        " Default syntax highlight by Vim enabled
set belloff=all      " No sound
set mouse=nv         " Mouse input enabled
set backspace=2      " Backspace enabled in insert mode
filetype plugin on   " I forgot what this is
set updatetime=100   " Make vim faster

" --------------------------------------------------------------------------------------------------
" File
" --------------------------------------------------------------------------------------------------

set encoding=utf-8  " Use UTF-8
set noswapfile      " Do not create unnecessary files
set nobackup        " Do not create unnecessary files
set noundofile      " Do not create unnecessary files
set hidden          " Switch buffer when it's not yet saved

set autoread                              " Reload when the file is modified
autocmd FocusGained,BufEnter * :checktime " detect external changes
autocmd FocusGained,BufEnter * :redraw!   " ..and automatically refresh screen

" --------------------------------------------------------------------------------------------------
" View
" --------------------------------------------------------------------------------------------------

set showtabline=2  " Show tab (even when no such plugin is enabled)
set showcmd        " Show input at right bottom coner
set ruler          " Show the line and column number of the cursor position
set showmatch      " Show matching pair of symbols
set number         " Show line numbers

" --------------------------------------------------------------------------------------------------
" Search
" --------------------------------------------------------------------------------------------------

set incsearch      " Incremental serach (ESC or C-[ to cancel)
set ignorecase     " Ignoring up/down case
set smartcase      " Only down case ignores up/down
set hlsearch       " Enables highlighting matches

" --------------------------------------------------------------------------------------------------
" Indent
" --------------------------------------------------------------------------------------------------

set foldmethod=indent
set foldlevel=99
set autoindent
set smartindent

" --------------------------------------------------------------------------------------------------
" Window commands
" --------------------------------------------------------------------------------------------------

" Split in direction (<C-w>[wasd])
nnoremap <silent> <C-w>w :leftabove split<CR>
nnoremap <silent> <C-w>a :leftabove vsplit<CR>
nnoremap <silent> <C-w>s :belowright split<CR>
nnoremap <silent> <C-w>d :belowright vsplit<CR>
nnoremap <silent> <C-w>m :belowright split<CR> :resize 6<CR>

" Split in direction (<Space>s[wasd])
nnoremap <silent> <Space>sw :leftabove split<CR>
nnoremap <silent> <Space>sa :leftabove vsplit<CR>
nnoremap <silent> <Space>ss :belowright split<CR>
nnoremap <silent> <Space>sd :belowright vsplit<CR>
nnoremap <silent> <Space>sm :belowright split<CR> :resize 6<CR>

" only, rotate
nnoremap <silent> <C-w>o :only<CR>
nnoremap <silent> <C-w>r :wincmd r<CR>
nnoremap <silent> <C-w>R :wincmd R<CR>

" new, close
nnoremap <silent> <C-w>x :quit<CR>
nnoremap <silent> <C-w>X :tabclose<CR>
nnoremap <silent> <C-w>q :quit<CR>
nnoremap <silent> <C-w>Q :tabclose<CR>
nnoremap <silent> <C-w>t :tabnew<CR>

nnoremap <silent> <C-w>] :tabnext<CR>
nnoremap <silent> <C-w>[ :tabprevious<CR>

nnoremap <silent> <C-w>h :wincmd h<CR>
nnoremap <silent> <C-w>j :wincmd j<CR>
nnoremap <silent> <C-w>k :wincmd k<CR>
nnoremap <silent> <C-w>l :wincmd l<CR>

nnoremap <silent> <C-w>< :5wincmd <<CR>
nnoremap <silent> <C-w>> :5wincmd ><CR>

nnoremap <silent> <C-w>H :wincmd H<CR>
nnoremap <silent> <C-w>J :wincmd J<CR>
nnoremap <silent> <C-w>K :wincmd K<CR>
nnoremap <silent> <C-w>L :wincmd L<CR>

nnoremap <silent> <C-w><C-h> :wincmd h<CR>
nnoremap <silent> <C-w><C-j> :wincmd j<CR>
nnoremap <silent> <C-w><C-k> :wincmd k<CR>
nnoremap <silent> <C-w><C-l> :wincmd l<CR>

nnoremap <silent> <C-w>= :wincmd =<CR>
nnoremap <silent> <C-w>p :wincmd p<CR>
nnoremap <silent> <C-w><C-p> :wincmd p<CR>

nnoremap <silent> <C-w>f :belowright vsplit<CR> :e <cfile><CR>
nnoremap <silent> <C-w>F :wincmd F<CR>

" --------------------------------------------------------------------------------------------------
" Key mappings
" --------------------------------------------------------------------------------------------------

nnoremap <silent> <C-x><C-c> :qa<CR>
nnoremap <silent> <C-x><C-s> :w<CR>
inoremap <silent> <C-s> <ESC>:w<CR>

nnoremap Y y$

nnoremap x "_x
nnoremap s "_s
vnoremap x "_x
vnoremap c "_c

nnoremap _ "_
vnoremap _ "_
nnoremap + "+
vnoremap + "+

inoremap <silent> <C-a> <C-o>^
inoremap <silent> <C-e> <C-o><End>
inoremap <silent> <C-b> <Left>
inoremap <silent> <C-f> <Right>
inoremap <silent> <C-d> <Delete>
inoremap <silent> <C-h> <BackSpace>

cnoremap <silent> <C-a> <Home>
cnoremap <silent> <C-e> <End><C-l>
cnoremap <silent> <C-b> <Left><C-l>
cnoremap <silent> <C-f> <Right><C-l>
cnoremap <silent> <C-d> <Del>

nnoremap [l :Lprevious<CR>
nnoremap ]l :Lnext<CR>
nnoremap [L :Lfirst<CR>
nnoremap ]L :Llast<CR>

nnoremap <silent> [t :tabprevious<CR>
nnoremap <silent> ]t :tabnext<CR>
nnoremap <silent> [T :tabfirst<CR>
nnoremap <silent> ]T :tabend<CR>

nnoremap <silent> [z z-
nnoremap <silent> ]z z<Enter>

nnoremap <silent> [d <C-u>
nnoremap <silent> ]d <C-d>

function! SmartWinNext()
  let start_buffer = bufnr('%')
  wincmd w
  while &buftype ==# 'quickfix' && bufnr('%') != start_buffer
    wincmd w
  endwhile
endfunction

function! SmartWinPrev()
  let start_buffer = bufnr('%')
  wincmd W
  while &buftype ==# 'quickfix' && bufnr('%') != start_buffer
    wincmd W
  endwhile
endfunction

nnoremap <silent> [w :call SmartWinPrev()<CR>
nnoremap <silent> ]w :call SmartWinNext()<CR>

" --------------------------------------------------------------------------------------------------
" Japanese
" --------------------------------------------------------------------------------------------------

let g:surround_{char2nr("「")} = "「 \r 」"
let g:surround_{char2nr("」")} = "「\r」"
let g:surround_{char2nr("『")} = "『 \r 』"
let g:surround_{char2nr("』")} = "『\r』"
let g:surround_{char2nr("【")} = "【 \r 】"
let g:surround_{char2nr("】")} = "【\r】"
let g:surround_{char2nr("（")} = "（ \r ）"
let g:surround_{char2nr("）")} = "（\r）"
let g:surround_{char2nr("＜")} = "＜ \r ＞"
let g:surround_{char2nr("＞")} = "＜\r＞"
let g:surround_{char2nr("《")} = "《 \r 》"
let g:surround_{char2nr("》")} = "《\r》"
let g:surround_{char2nr("｛")} = "｛ \r ｝"
let g:surround_{char2nr("｝")} = "｛\r｝"

set matchpairs& matchpairs+=<:>
set matchpairs+=「:」,『:』,（:）,【:】,《:》,〈:〉,［:］,‘:’,“:”

" When joining, do not add white spaces for multi bytes characters
set formatoptions+=mMj

" force fixed length for symbols
" set ambiwidth=double

function! s:MapFTR(key, char)
  for cmd in ['f', 'F', 't', 'T', 'r']
    execute 'noremap <silent> ' . cmd . a:key . ' ' . cmd . a:char
  endfor
endfunction

function! s:UnmapFTR(key)
  for cmd in ['f', 'F', 't', 'T', 'r']
    execute 'nunmap <silent> ' . cmd . a:key
  endfor
endfunction

" REMARK: We have to type them within `timeoutlen` ??
call <SID>MapFTR('<C-j>', '、')
call <SID>MapFTR('<C-l>', '。')

nnoremap [s F、
nnoremap ]s f、
nnoremap [d F。
nnoremap ]d f。

nnoremap [S T、
nnoremap ]S t、
nnoremap [D T。
nnoremap ]D t。

