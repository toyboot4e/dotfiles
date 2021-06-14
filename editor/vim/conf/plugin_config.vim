" plugin_config.vim

filetype plugin indent on

" --------------------------------------------------------------------------------
" UTILITIES

" ----------------------------------------
" surround.vim
let g:surround_{char2nr("`")} = "`\r`"

" --------------------------------------------------------------------------------
" MARKUPS/OBJECT NOTATIONS

" ----------------------------------------
" markdown
let g:table_mode_corner = '|'

let g:vim_markdown_folding_style_pythonic = 1
let g:vim_markdown_conceal = 0
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_toml_frontmatter = 1
let g:vim_markdown_json_frontmatter = 1
set conceallevel=0

" ----------------------------------------
" object notation
let g:csv_comment = '//'

" --------------------------------------------------------------------------------
" Passive view

" git-gutter
" (git-gutter:modified ((t (:background "#c0b18b" :foreground "#2f2f2f"))))
" (git-gutter:added    ((t (:background "#84edb9" :foreground "#2f2f2f"))))
" (git-gutter:deleted  ((t (:background "#d75f5f" :foreground "#2f2f2f"))))

" TODO: set background color
" highlight GitGutterAdd    guifg=#009900 ctermfg=2
" highlight GitGutterChange guifg=#bbbb00 ctermfg=3
" highlight GitGutterDelete guifg=#ff2222 ctermfg=1

" --------------------------------------------------------------------------------
" WIDGETS

" ----------------------------------------
" Floating terminal (floaterm; requires terminal feature)

let g:floaterm_width = 1.0
let g:floaterm_height = 1.0
let g:floaterm_position = 'center'
" let g:floaterm_gitcommit = 'floaterm'

hi FloatermNF guibg=none
hi FloatermBorderNF guibg=black guifg=cyan

let g:floaterm_gitcommit = 1  " use `git commit` without a nested terminal

function s:floatermSettings()
    setlocal number
    " more settings
endfunction

autocmd FileType floaterm call s:floatermSettings()

" ----------------------------------------
" NERDTree
let NERDTreeShowHidden = 1     " Show hidden files
let g:NERDTreeNaturalSort = 1  " Just turn it on!
let NERDTreeQuitOnOpen = 0     " Keep it opend after opening a file from it
let g:NERDTreeWinPos = 'right' " Show it on the right side in a certain size
let g:NERDTreeWinSize = 25     " in a certain size

" WE HAVE TO MANUALLY SPECIFY WHICH FILES TO BE IGNORED
let NERDTreeIgnore = [
    \ '\.git$', '\.DS_Store$', 'node_modules',
    \ '**\.o$',
    \ '\.ccls-cache$', '\.mypy_cache$', 'target',
    \ ]

" ----------------------------------------
" Goyo (zen mode)
let g:goyo_margin_top = 0
let g:goyo_margin_bottom = 0
let g:goyo_height = '100%'
" note that Goyo always have some margins as its limitation

" callbacks
function! s:goyo_enter()
    silent! W3mSyntaxOn() " consider w3m window
endfunction

function! s:goyo_leave()
    return
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

" --------------------------------------------------------------------------------
" fzf

" default settings
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit'
  \ }

" top down if reversed
let $FZF_DEFAULT_OPTS='--layout=reverse'

" ----------------------------------------
" commands

let g:make = 'gmake'
if exists('make')
    let g:make = 'make'
endif

let g:fzf_preview_filelist_command = 'git ls-files --exclude-standard'
if executable('rg')
    let g:fzf_preview_filelist_command = 'rg --files --hidden --follow --no-messages -g \!"* *"'

    " RG
    function! RipgrepFzf(query, fullscreen)
    let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
    let initial_command = printf(command_fmt, shellescape(a:query))
    let reload_command = printf(command_fmt, '{q}')
    let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
    call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
  endfunction

    command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)
endif

let $FZF_DEFAULT_COMMAND = 'fd -t f -H'

" ----------------------------------------
" Floating window (NeoVim only)

if has('nvim-0.4.0')
    let g:fzf_layout = { 'window': 'call FloatingFZF()' }
    function! FloatingFZF()
        let buf = nvim_create_buf(v:false, v:true)
        let height = float2nr(&lines * 0.9)
        let width = float2nr(&columns * 0.9)
        let horizontal = float2nr((&columns - width) / 2)
        " let vertical = float2nr((&rows - height) / 2)
        let vertical = 3
        let opts = {
              \ 'relative': 'editor',
              \ 'row': vertical,
              \ 'col': horizontal,
              \ 'width': width,
              \ 'height': height
              \ }
        call nvim_open_win(buf, v:true, opts)
    endfunction
endif

" ----------------------------------------
" View

let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

function! s:fzf_statusline()
  " Override statusline as you like
  highlight fzf1 ctermfg=161 ctermbg=251
  highlight fzf2 ctermfg=23 ctermbg=251
  highlight fzf3 ctermfg=237 ctermbg=251
  setlocal statusline=%#fzf1#\ >\ %#fzf2#fz%#fzf3#f
endfunction

autocmd! User FzfStatusLine call <SID>fzf_statusline()

" --------------------------------------------------------------------------------
" AIRLINE (NOT in use; vim-wintabs is prefererd in this configuration)

let g:airline_theme = 'bubblegum'                          " choose a theme you like
let g:airline_powerline_fonts = 1                          " if you have installed a powerline font or a NERD font

let g:airline#extensions#whitespace#mixed_indent_algo = 1  " Better look in neovim

if g:use_wintabs
    let g:airline#extensions#tabline#enabled = 0
else
    let g:airline#extensions#tabline#enabled = 1
endif

let g:airline#extensions#tabline#show_tabs = 0             " show tabs only if there's more than one
let g:airline#extensions#ale#enabled = 1

let g:airline#extensions#tabline#show_splits = 0           " hide the right side of the tab bar
let g:airline#extensions#tabline#show_tab_type = 0         " hide the right side of the tab bar
let g:airline#extensions#tabline#show_close_button = 0     " hide the right side of the tab bar

let g:airline_mode_map = {
    \ '__' : '-',
    \'c'  : 'C',
    \ 'i'  : 'I',
    \ 'ic' : 'I',
    \ 'ix' : 'I',
    \ 'n'  : 'N',
    \ 'ni' : 'N',
    \ 'no' : 'N',
    \ 'R'  : 'R',
    \ 'Rv' : 'R',
    \ 's'  : 'S',
    \ 'S'  : 'S',
    \ '^S' : 'S',
    \ 't'  : 'T',
    \ 'v'  : 'V',
    \ 'V'  : 'V',
    \ '^V' : 'V',
    \ }

" let g:airline_extensions = []

" --------------------------------------------------------------------------------
" vim-wintabs


hi link WintabsEmpty      TabLineFill
hi link WintabsActive     TabLineSel
hi link WintabsInactive   TabLine
hi link WintabsArrow      TabLine
hi link WintabsActiveNC   TabLine
hi link WintabsInactiveNC TabLine

" '': current window, 'useopen': window that has the buffer, 'usetab': open in this window
let g:wintabs_switchbuf = 'usetab'
" Never close the window with `:WintabsClose`. Now, it only closes a buffer
let g:wintabs_autoclose = 0

" TODO: make it work
function! SyncBuffersPerTab()
    " remember the buffer information (*fold info lost*)
    let l:buf = bufnr('%')
    if !buflisted(l:buf)
        return
    endif
    let l:view = winsaveview()

    " sync
    WintabsOnly
    WintabsClose
    WintabsAll
    execute "buffer " . l:buf
    call winrestview(l:view)
endfunction

if g:use_wintabs
    augroup wintabs_support
        " ASSUMING THAT WE'RE NOT RESTORING A SESSION,
        " let's attach all files to a startup window
        autocmd VimEnter * WintabsAllBuffers

        " Use per-tab buffer tabline (BROKEN)
        " autocmd WinEnter * call SyncBuffersPerTab()
        " autocmd TabEnter *
    augroup END
endif

" ??
" let g:wintabs_powerline_arrow_left = " \u25c0 "
" let g:wintabs_powerline_arrow_right = " \u25b6 "
" let g:wintabs_powerline_sep_buffer_transition = "ue0b0"
" let g:wintabs_powerline_sep_buffer = "\ue0b1"
" let g:wintabs_powerline_sep_tab_transition = "\ue0b2"
" let g:wintabs_powerline_sep_tab = "\ue0b3"

