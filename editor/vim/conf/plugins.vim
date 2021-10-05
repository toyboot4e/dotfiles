let g:netrw_browsex_viewer = 'open'

" junegunn/vim-plug (`Minimalist Vim Plugin Manager`)
silent! if plug#begin('~/.vim/plugged')

" REMARK: if you want tabline, do: `:CocInstall coc-tabnine`

" --------------------------------------------------------------------------------
" Environment

let g:histexclude = { ':': '^w$|^q$^wa$' }
Plug 'itchyny/vim-histexclude'

" --------------------------------------------------------------------------------
" LANGUAGE SERVER

Plug 'editorconfig/editorconfig-vim'

" ----------------------------------------
" coc.nvim

Plug 'neoclide/coc.nvim', {'branch': 'release'}

command! -nargs=0 Prettier :CocCommand prettier.formatFile
command! -nargs=? Fold :call     CocAction('fold', <f-args>)
highlight CocErrorSign ctermfg=15 ctermbg=196
highlight CocWarningSign ctermfg=0 ctermbg=172

" ----------------------------------------
" nvim-treesitter

if has('nvim')
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
endif

" ----------------------------------------
" omnisharp-vim + ALE (we let ALE only work for C# as declared in `omni.vim`)

Plug 'OmniSharp/omnisharp-vim'
Plug 'dense-analysis/ale'
" Plug 'prabirshrestha/asyncomplete.vim'
" Plug 'prabirshrestha/asyncomplete-lsp.vim'
let g:OmniSharp_server_stdio = 1
let g:OmniSharp_server_use_mono = 1

" let g:OmniSharp_proc_debug = 1

" Cursor-based error (location navigation
Plug 'msrose/vim-perpetuloc'

" ----------------------------------------
" vim-lsp
"
" Plug 'prabirshrestha/vim-lsp'
" vim-lsp-settings: https://github.com/mattn/vim-lsp-settings
" Plug 'prabirshrestha/async.vim'
" Plug 'mattn/vim-lsp-settings'

" --------------------------------------------------------------------------------
" CODING

" ----------------------------------------
" QUOTES

" REMARK: automatically closing of quotes ('", brackets {}, Paranthesis (), ..)
" Plug 'Raimondi/delimitMate'

" ----------------------------------------
" SNIPPETS

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger='<c-k>'
let g:UltiSnipsEditSplit="vertical"

" TODO: this is very heavt to load

" snippet engine
" Plug 'SirVer/ultisnips'

" snippet data
" Plug 'honza/vim-snippets'
" Plug 'zooxyt/Ultisnips-rust'

" ----------------------------------------
" COMMENTING

" comment out
let g:NERDCreateDefaultMappings = 0
Plug 'scrooloose/nerdcommenter'
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDDefaultAlign = 'left'
" let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1
let g:NERDToggleCheckAllLines = 1

" --------------------------------------------------------------------------------
" COLOR SCHEMES

Plug 'flazz/vim-colorschemes'

" --------------------------------------------------------------------------------
" INTERNALS

Plug 'vim-scripts/visualrepeat'

" --------------------------------------------------------------------------------
" KEY MAPPINGS

" Trete whitespace lines as paragraph boundaries:
Plug 'dbakker/vim-paragraph-motion'

" --------------------------------------------------------------------------------
" KEY BINDINGS

" Open URL with gX (X = B: in browser, G: in google, W: in wiki)
Plug 'dhruvasagar/vim-open-url'

" Use `v` to expand/shrink the selected region in visual mode
Plug 'terryma/vim-expand-region'

" Use `S` as a motion about `surrounding`
Plug 'tpope/vim-surround'

" Enables vim-surround to be repeated with '.'
Plug 'tpope/vim-repeat'

" add `[]` related actions c.f. https://github.com/tpope/vim-unimpaired
Plug 'tpope/vim-unimpaired'
" TODO: maybe remove unimpaired.vim and set them manually

" crX to change case (e.g. X=s: snake_case)
Plug 'tpope/vim-abolish'

" --------------------------------------------------------------------------------
" Window management

" ----------------------------------------
" Resize-window mode
" available modes: (f: focus, m: move, r: resize)
Plug 'simeji/winresizer'
let g:winresizer_vert_resize = 5

" ----------------------------------------
" Zoom-in mode
let g:loaded_zoom = 1 " disable default binding `<C-w>m`
Plug 'dhruvasagar/vim-zoom'

" --------------------------------------------------------------------------------
" Writing

" zen mode (per screen only)
Plug 'junegunn/goyo.vim'

" check grammer with `:GrammarousCheck`
Plug 'rhysd/vim-grammarous'

" --------------------------------------------------------------------------------
" Navigations

" ----------------------------------------
" `:Rooter`
let g:rooter_manual_only = 1
Plug 'airblade/vim-rooter'

" ----------------------------------------
" BUFFER COMMANSD

" :Rename, :Move, etc. https://github.com/tpope/vim-eunuch
Plug 'tpope/vim-eunuch'

" :Bufonly :Bonly
Plug 'schickling/vim-bufonly'

" :BD, new :bd while keeping the window
Plug 'qpkorr/vim-bufkill'

" ----------------------------------------
" SEARCH COMMANDS

" REMARK: `:SearchTasks .` or `SearchTasksGrep` to list TODO and FIXME
Plug 'gilsondev/searchtasks.vim'

" See xterm colors with :XtermColorTable
Plug 'guns/xterm-color-table.vim'

" ----------------------------------------
" GIT COMMANDS

" :Gxx to use git commands, :Gbrowse to open current file on GitHub
Plug 'tpope/vim-fugitive'

" Project-based Git integration
Plug 'jreybert/vimagit'

" --------------------------------------------------------------------------------
" Japanese

" word-based cursor movement in Japanese (?)
Plug 'deton/jasegment.vim'

" IME switcher (macOS) (not using?)
Plug 'mitsuse/swim'

" --------------------------------------------------------------------------------
" PERSISTANCE

" Crates `./Session.vim` using `:mksession`
Plug 'tpope/vim-obsession'

" Save cursor position
Plug 'farmergreg/vim-lastplace'

" --------------------------------------------------------------------------------
" VIEW (PASSIVE ENHANCEMENTS)

" REMARK: `]c` and `[c` to go to next/previous change (vim-gitgutter)

" Plug 'liuchengxu/vim-which-key'        " shows keys after a prefix

Plug 'mhinz/vim-startify'              " start screen
Plug 'Yggdroot/indentLine'             " indent line
Plug 'bronson/vim-trailing-whitespace' " highlights trailing whitespaces
Plug 'luochen1990/rainbow'             " Colorizes (nested) parentheses
Plug 'airblade/vim-gitgutter'          " shows git diff at line numbers.
Plug 'ap/vim-css-color'                " highlights HTML color codes e.g. #d13642
let g:WebDevIconsUnicodeDecorateFolderNodes = 1
let g:DevIconsEnableFoldersOpenClose = 1
Plug 'ryanoasis/vim-devicons'          " icons (REMARK: NERD fonts must be installed)
" Plug 'statox/FYT.vim'                  " highlight on yank

" --------------------------------------------------------------------------------
" TABLINE and STATUS LINE

Plug 'vim-airline/vim-airline'         " status line and tabline
Plug 'vim-airline/vim-airline-themes'  " status line and tabline

if g:use_wintabs
    Plug 'zefei/vim-wintabs'
    Plug 'zefei/vim-wintabs-powerline'
endif

" ----------------------------------------
" Floating terminal (requires terminal feature)

Plug 'voldikss/vim-floaterm'  " :FloatermToggle

" ----------------------------------------
" NERDTree (explorer in sidebar)

Plug 'scrooloose/nerdtree'     " The file browser
Plug 'jistr/vim-nerdtree-tabs' " share NERDTree among tabs
" Plug 'Xuyuanp/nerdtree-git-plugin' " NERDTree with git status (extreamly slow)

Plug 'lambdalisue/nerdfont.vim'
Plug 'lambdalisue/glyph-palette.vim'
Plug 'lambdalisue/fern.vim'
Plug 'lambdalisue/fern-renderer-nerdfont.vim'
Plug 'lambdalisue/fern-git-status.vim'
Plug 'lambdalisue/gina.vim'

" colorize nerdfonts with glyph-palette
augroup my-glyph-palette
  autocmd! *
  autocmd FileType fern call glyph_palette#apply()
  autocmd FileType nerdtree,startify call glyph_palette#apply()
augroup END

let g:fern#renderer = "nerdfont"

Plug 'antoinemadec/FixCursorHold.nvim'

" in millisecond, used for both CursorHold and CursorHoldI,
" use updatetime instead if not defined
let g:cursorhold_updatetime = 100

" ----------------------------------------
" fzf

" TIP: <C-c> to exit fzf (as a terminal process)

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'yuki-ycino/fzf-preview.vim', { 'branch': 'release', 'do': ':UpdateRemotePlugins' }
Plug 'antoinemadec/coc-fzf'

" --------------------------------------------------------------------------------
" BROWSING

" ----------------------------------------
" BROWSING FILES

" :Np
" Plug 'mcchrish/nnn.vim'

" :Ranger
" Plug 'francoiscabrol/ranger.vim'

" ----------------------------------------
" Browsing web

if executable('w3m')
    Plug 'yuratomo/w3m.vim'
endif

" ----------------------------------------
" Translation

let g:translate_source = "en"
let g:translate_target = "ja"
let g:translate_winsize = 10

Plug 'skanehira/translate.vim'

" --------------------------------------------------------------------------------
" LANGUAGE SPECIFIC SETTINGS

" Language packs
" Plug 'sheerun/vim-polyglot'

" ----------------------------------------
" Programmiing languages

Plug 'Tetralux/odin.vim'
Plug 'wlangstroth/vim-racket'

" ----------------------------------------
" Markup langiages

let g:asciidoctor_folding = 1
let g:asciidoctor_fold_options = 0
Plug 'habamax/vim-asciidoctor'

let g:vim_markdown_conceal = 0

" :TableFormat
Plug 'plasticboy/vim-markdown'
let g:vim_markdown_folding_disabled = 1
Plug 'godlygeek/tabular'

" ----------------------------------------
" todo.txt
"
Plug 'freitass/todo.txt-vim'

" ----------------------------------------
" Object notations
"
Plug 'ron-rs/ron.vim'
Plug 'neoclide/jsonc.vim'
Plug 'chrisbra/csv.vim'
Plug 'mechatroner/rainbow_csv'

" --------------------------------------------------------------------------------
" DISABLED

" show registers in interaction
" Plug 'junegunn/vim-peekaboo'

call plug#end()
endif

