" omni.vim
" sets up omnisharp.vim and ALE
"
" TODO: refactor
" REMARK: It looks like ALE commands don't work. It just prints errors

" --------------------------------------------------------------------------------
" Setup

" Requirements
filetype indent plugin on
let g:OmniSharp_timeout = 5
set completeopt=longest,menuone,preview
set previewheight=5
let g:OmniSharp_want_snippet=1

" ALE setup
let g:ale_linters_explicit = 1 " ALE should only work for C#
let g:ale_linters = { 'cs': ['OmniSharp'] }

" use asynchronous server interactions
let g:OmniSharp_server_stdio = 1
let g:OmniSharp_server_use_mono = 1

let g:ale_open_list = 1
let g:ale_keep_list_window_open = 1
let g:ale_list_window_size = 6

" --------------------------------------------------------------------------------
" Preferences

let g:ale_fix_on_save = 1
let g:OmniSharp_selector_ui = 'fzf'

" Fetch full documentation during omnicomplete requests.
" By default, only Type/Method signatures are fetched. Full documentation can
" still be fetched when you need it with the :OmniSharpDocumentation command.
let g:omnicomplete_fetch_full_documentation = 0

" Update semantic highlighting after all text changes
let g:OmniSharp_highlight_types = 3
" Update semantic highlighting on BufEnter and InsertLeave
" let g:OmniSharp_highlight_types = 2

" --------------------------------------------------------------------------------
" Key mappings

augroup omnisharp_commands
    autocmd!

    " ALE navigations
    nmap [g :Lprevious<CR>
    nmap ]g :Lnext<CR>
    nmap [G :Lfirst<CR>
    nmap ]G :Llast<CR>

    " save on format
    autocmd FileType cs autocmd BufWritePre :OmniSharpCodeFormat<CR>

    " Show type information automatically when the cursor stops moving.
    " Note that the type is echoed to the Vim command line, and will overwrite
    " any other messages in this space including e.g. ALE linting messages.
    " autocmd CursorHold *.cs OmniSharpTypeLookup

    " ALE cursor
    " autocmd FileType cs nnoremap <buffer> gd :ALEGoToDefinition<CR>
    " autocmd FileType cs nmap <buffer> K :ALEHover<CR>
    " autocmd FileType cs nmap <buffer> <f12> :ALEGoToDefinition<CR>
    " autocmd FileType cs nmap <buffer> <f2> :ALERename<CR>

    autocmd FileType cs nnoremap <buffer> gd :OmniSharpGotoDefinition<CR>
    autocmd FileType cs nmap <buffer> <f2> :OmniSharpRename<CR>
    autocmd FileType cs nmap <buffer> <f12> :OmniSharpGotoDefinition<CR>

    " Navigate up and down by method/property/field
    autocmd FileType cs nnoremap <buffer> <C-k> :OmniSharpNavigateUp<CR>
    autocmd FileType cs nnoremap <buffer> <C-j> :OmniSharpNavigateDown<CR>

    " OmniSharp commands over the project
    autocmd FileType cs nnoremap <buffer> <Leader>oh :OmniSharpSignatureHelp<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>od :OmniSharpDocumentation<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>om :OmniSharpFindMembers<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>oc :OmniSharpGlobalCodeCheck<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>of :OmniSharpCodeFormat<CR>

    " autocmd FileType cs nnoremap <buffer> <Leader>fx :OmniSharpFixUsings<CR>
    " autocmd FileType cs nnoremap <buffer> <Leader>tt :OmniSharpTypeLookup<CR>
    " autocmd FileType cs nnoremap <buffer> gd :OmniSharpDocumentation<CR>
    " autocmd FileType cs nnoremap <buffer> <Leader>dc :OmniSharpDocumentation<CR>
    " autocmd FileType cs nnoremap <buffer> <C-\> :OmniSharpSignatureHelp<CR>
    " autocmd FileType cs inoremap <buffer> <C-\> <C-o>:OmniSharpSignatureHelp<CR>

    " Contextual code actions (uses fzf, CtrlP or unite.vim when available)
    " autocmd FileType cs nnoremap <Leader><Space> :OmniSharpGetCodeActions<CR>
    " Run code actions with text selected in visual mode to extract method
    " autocmd FileType cs xnoremap <Leader><Space> :call OmniSharp#GetCodeActions('visual')<CR>

augroup END

