" .vimrc for `git` etc.

nnoremap <silent> <C-x><C-c> :qa<CR>
nnoremap <silent> <C-x><C-s> :w<CR>
inoremap <silent> <C-s> <ESC>:w<CR>

" Misc

inoremap jk <ESC>
vnoremap jk <Esc>
onoremap jk <Esc>
cnoremap jk <C-C><Esc>

nnoremap Y y$

nnoremap x "_x
nnoremap s "_s
vnoremap x "_x
vnoremap c "_c

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

