" mappings.vim

" TIP: search :: for going to a next prefix
" TIP: `:reg` to check registers
" TIP: `:Maps` to check mappings (fzf.vim)

" --------------------------------------------------------------------------------
" AVOID-ESC (based on:https://vim.fandom.com/wiki/Avoid_the_escape_key)

inoremap jk <ESC>
vnoremap jk <Esc>
onoremap jk <Esc>
cnoremap jk <C-C><Esc>

" NOTE: this is mainly for quitting fzf in a floating window
tnoremap jk <C-c>
" This maps `jk` to ESC-like
" tnoremap jk <C-\><C-n>

" --------------------------------------------------------------------------------
" CHANGES TO DEFAULTS

" apply word wrapping to range
nnoremap Q gq}

" ----------------------------------------
" centerize the cursor on serach

nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap * *zz
nnoremap g* g*zz
nnoremap g* g*zz
" todo: do `zz` on first search, too

" ----------------------------------------
" scroll while keeping the cursor position
nnoremap <C-f> <C-d><C-d>
nnoremap <C-b> <C-u><C-u>

" ----------------------------------------
" YANK

"  Fix Y from yy to y$
nnoremap Y y$

" Just delete
nnoremap x "_x
nnoremap s "_s
vnoremap x "_x
vnoremap c "_c

" ----------------------------------------
" register
nnoremap _ "_
vnoremap _ "_
nnoremap + "+
vnoremap + "+

" --------------------------------------------------------------------------------
" NORMAL MODE MAPPINGS

" FIXME: not working
" nnoremap <silent> <C-x><C-c> :wa<CR>:qa<CR>
nnoremap <silent> <C-x><C-c> :qa<CR>
nnoremap <silent> <C-x><C-s> :w<CR>
" I love this.. even easier to exit insert mode
inoremap <silent> <C-s> <ESC>:w<CR>

" --------------------------------------------------------------------------------
" INSERT MODE MAPPINGS

" ----------------------------------------
" EMACS-LIKE MAPPINGS

inoremap <silent> <C-a> <C-o>^
inoremap <silent> <C-e> <C-o><End>
inoremap <silent> <C-b> <Left>
inoremap <silent> <C-f> <Right>
inoremap <silent> <C-d> <Delete>
inoremap <silent> <C-h> <BackSpace>

" ----------------------------------------
" up, down
inoremap <silent> <C-p> <Up>
inoremap <silent> <C-n> <Down>

" ----------------------------------------
" C-o

" (prefer the default one to delete the last word)
" inoremap <silent> <C-w> <C-o><C-w>

inoremap <silent> <C-g> <C-o>g
inoremap <silent> <C-l> <C-o><C-l>
" inoremap <silent> <c-p> <C-o>p

" --------------------------------------------------------------------------------
" COMMAND LINE MODE MAPPINGS

" ----------------------------------------
" EMACS-LIKE MAPPINGS

cnoremap <silent> <C-a> <Home>
cnoremap <silent> <C-e> <End><C-l>
cnoremap <silent> <C-b> <Left><C-l>
cnoremap <silent> <C-f> <Right><C-l>
cnoremap <silent> <C-d> <Del>
" cnoremap <silent> <C-h> <BackSpace>
" ^ included in the defaults

" --------------------------------------------------------------------------------
" VISUAL-MODE MAPPINGS

" ----------------------------------------
" expand_region
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

" --------------------------------------------------------------------------------
" <Space>t :: Terminal Operations

" ----------------------------------------
" Open terminal in current window
nnoremap <silent> <Space>tt :term<CR> :setlocal nonumber<CR> i

" ----------------------------------------
" Split into a terminal and LoxInstance enter insert mode
nnoremap <silent> <Space>tw :leftabove split<CR> :term<CR> :setlocal nonumber<CR> i
nnoremap <silent> <Space>ta :leftabove vsplit<CR> :term<CR> :setlocal nonumber<CR> i
nnoremap <silent> <Space>ts :belowright split<CR> :term<CR> :setlocal nonumber<CR> i
nnoremap <silent> <Space>td :belowright vsplit<CR> :term<CR> :setlocal nonumber<CR> i
nnoremap <silent> <Space>tm :belowright split<CR> :resize 6<CR> :term<CR> :setlocal nonumber<CR> i

" --------------------------------------------------------------------------------
" TERMINAL MODE MAPPINGS

" ----------------------------------------
" Fix
tnoremap <silent> <ESC> <C-\><C-n>
tnoremap <silent> <C-[> <C-\><C-n>

" ----------------------------------------
" Living in terminal mode (NOT ENTERS C-w why??)
tnoremap <silent> <C-w> <C-\><C-n><C-w>

" --------------------------------------------------------------------------------
" vim-wintabs (overwrites some commands)

" kill buffer
nnoremap <silent> <Space>k :BD!<CR>
nnoremap <silent> <Space>L :bd!<CR>

nnoremap <silent> <Space>q :quit<CR>

if !g:use_wintabs
    nnoremap <silent> ]b :bnext<CR>
    nnoremap <silent> [b :bprevious<CR>
    nnoremap <silent> [B :bfirst<CR>
    nnoremap <silent> ]B :blast<CR>

else
    nnoremap <silent> ]b :WintabsNext<CR>
    nnoremap <silent> [b :WintabsPrevious<CR>
    nnoremap <silent> [B :bfirst<CR>
    nnoremap <silent> ]B :blast<CR>


    " NOTE: you can't undo closing t tab
    nnoremap <silent> <Space>wu :WintabsUndo<CR>
    " Attach all buffers to this window
    nnoremap <silent> <Space>wt :WintabsAll<CR>
    " Attach all buffers in this tab to this window
    nnoremap <silent> <Space>wa :WintabsAllBuffers<CR>
    nnoremap <silent> <Space>wc :close!<CR>

    " nnoremap <silent> <Space>ws :call SyncBuffersPerTab()<CR>
    nnoremap <silent> <Space>ws :WintabsDo call SyncBuffersPerTab()<CR>
endif

" --------------------------------------------------------------------------------
" [] :: next/previous or on/off

""""""""""""""""""""""""""""""""""""""""
" REMARK: From vim-unimpaired:
" - []e to swap lines
" - []<Space> to insert empty line
" - []oX to toggle option X (e.g. `]on`: hide line numbers)

" REMARK: From vim-fugitive:
" - []c to go to next/previous change

" REMARK: From coc.nvim:
" - []g to go to next/previous diagnostic

" REMARK: From omnisharp.vim:
" - C-j, C-k to navigate to next/previous item

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
nnoremap <silent> [z z-
nnoremap <silent> ]z z<Enter>
nnoremap <silent> [d <C-u>
nnoremap <silent> ]d <C-d>

" cursor based location list navigation (see: https://github.com/msrose/vim-perpetuloc
nnoremap [l :Lprevious<CR>
nnoremap ]l :Lnext<CR>
nnoremap [L :Lfirst<CR>
nnoremap ]L :Llast<CR>

nnoremap <silent> [t :tabprevious<CR>
nnoremap <silent> ]t :tabnext<CR>
nnoremap <silent> [T :tabfirst<CR>
nnoremap <silent> ]T :tabend<CR>

" I'm using `autocmd` to skip undesired buffers (see the end of `.vimrc`)
" Dependent on vim-wintabs:

" --------------------------------------------------------------------------------
" <Space>

" ----------------------------------------
" BUFFER/WINDOW

" Force deleting the buffer (useful for terminal, too) leaving the window
" nnoremap <silent> <Space>c :BD!<CR>
" Force deleting the buffer (useful for terminal, too) and close the window
" nnoremap <silent> <Space>C :bd!<CR>
" Just close the window (it may end Vim. Prefer :close if you don't want to)
" nnoremap <silent> <Space>q :quit<CR>

" --------------------------------------------------------------------------------
" <C-w> :: WINDOW COMMANDS

" REMARK: use <C-w>e to enter resizing mode (set in `plugin_config_preload.vim`)
" REMARK: use [b, ]b to focus next/previous buffer
" REMARK: use [t, ]t to focus next/previous tab
" REMARK: use [w, ]w to focus next/previous window

" ----------------------------------------
" Additions to the window commands

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

" tab focus
nnoremap <silent> <C-w>] :tabnext<CR>
nnoremap <silent> <C-w>[ :tabprevious<CR>

" TODO: {} to swap tabs or buffers

" ----------------------------------------

" c.f. `:help window-moving`
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

" --------------------------------------------------------------------------------
" <Space><Space> :: TOGGLE MODE

" ----------------------------------------
" Floating terminal

let g:floaterm_keymap_toggle = '<Space><Space>t'
" nnoremap <silent> <Space><Space>t :FloatermToggle  --height=0.5 --width=0.7 --position=topleft --autoclose=2<CR>
" inoremap <silent> C-t :FloatermToggle<CR>
" nnoremap <silent> <Space><Space>f :FloatermNew ranger<CR>

" ----------------------------------------
" Sidebar (NERDTree)

" " reveal the file
" nnoremap <silent> <Space>nf :NERDTreeFind<CR>
" nnoremap <silent> <Space>nn :NERDTreeTabsToggle<CR>
" nnoremap <silent> <Space>nr :NERDTreeRefreshRoot<CR>

nnoremap <silent> <Space>nn :Fern . -drawer -width=24 -toggle -reveal=%<CR> <C-w>=
nnoremap <silent> <Space>nf :Fern . -drawer -width=24 -toggle<CR> <C-w>=

" ----------------------------------------
" Resizing (winresizer)

nnoremap <silent> <C-w>z :call zoom#toggle()<CR>
nnoremap <silent> <Space><Space>z :call zoom#toggle()<CR>

" ----------------------------------------
" Zen (Goyo)

" Toggles Goyo considering the width of NERDTree. Requires NERDTreeTabs,
" which makes NERDTree visible even when `:Goyo` is executed (HACK)
function! GoyoWithNERD()
    let g:goyo_width = g:goyo_preferred_width
    if g:NERDTree.IsOpen()
        let g:goyo_width += g:NERDTreeWinSize
    end
    Goyo
endfunction
command! GoyoWithNERD :call GoyoWithNERD()

function! GoyoWithNERDRefresh()
    GoyoWithNERD
    GoyoWithNERD
endfunction
command! GoyoWithNERDRefresh :call GoyoWithNERDRefresh()

nnoremap <silent> <Space><Space>g :call GoyoWithNERD()<CR>
" nnoremap <silent> <Space><Space>f :call GoyoWithNERDRefresh()<CR>

" change preferred width
" nnoremap <expr> <Space>zw ':execute :let g:goyo_preferred_width=' . g:goyo_preferred_width

" --------------------------------------------------------------------------------
" FZF

" TIP: Use <C-c> to quit

" ----------------------------------------
" Moving around

" nnoremap <C-P> :FzfPreviewDirectoryFiles<CR>
nnoremap <C-p> :Files<CR>

nnoremap <Space>ff :FzfPreviewDirectoryFiles<CR>
nnoremap <Space>fF :Files<CR>
nnoremap <Space>fb :Buffers<CR>

nnoremap <Space>gt :CocCommand fzf-preview.Buffers<CR>
nnoremap <Space>gT :CocCommand fzf-preview.DirectoryFiles<CR>
nnoremap <Space>gr :rg<CR>
nnoremap <Space>gR :RG<CR>
nnoremap <Space>gb :Buffers<CR>
" nnoremap gr :CocCommand fzf-preview.ProjectGrep

nnoremap <Space>gl :CocCommand fzf-preview.Lines<CR>
nnoremap <Space>gs :CocCommand fzf-preview.GitStatus<CR>
nnoremap <Space>gc :BCommits<CR>

" --------------------------------------------------------------------------------
" Coding

" ----------------------------------------
" NERDCommenter

if has('win32')
  nmap <C-/>   <Plug>NERDCommenterToggle
  vmap <C-/>   <Plug>NERDCommenterToggle<CR>gv
  xmap <C-/>   <Plug>NERDCommenterToggle<CR>gv
else
  " REMARK: In non-windows OS, a slash is represented as `_` (for historical reason)
  nmap <C-_>   <Plug>NERDCommenterToggle
  vmap <C-_>   <Plug>NERDCommenterToggle<CR>gv
  xmap <C-_>   <Plug>NERDCommenterToggle<CR>gv
endif

" imap <Space>/ <C-o><Plug>NERDCommenterToggle # NOT WORKING
nmap <Space>/ <Plug>NERDCommenterToggle
vmap <Space>/ <Plug>NERDCommenterToggle

" --------------------------------------------------------------------------------
" SESSION

" save/load/delete
nnoremap <silent> <Space>ps :mksession ./Session.vim<CR>
nnoremap <silent> <Space>pl :source Session.vim<CR>
nnoremap <silent> <Space>pd :!rm Session.vim<CR>

" begin/end (requires obsession.vim: https://github.com/tpope/vim-obsession
nnoremap <silent> <Space>po :Obsess<CR>
nnoremap <silent> <Space>pp :Obsess!<CR>

" --------------------------------------------------------------------------------
" MARKDOWN

" TODO: Setting up language specigic settings (with autocmd?)
nnoremap <Space>mf :TableFormat<CR>

" --------------------------------------------------------------------------------
" ADDITIONAL COMMANDS

" toggle highlight with `<Space>h
nnoremap <silent><expr> <Space>h (&hls && v:hlsearch ? ':nohls' : ':set hls')."\n"

" --------------------------------------------------------------------------------
" ABBREVIATIONS

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

