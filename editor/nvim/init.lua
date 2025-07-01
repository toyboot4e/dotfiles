-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end

vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.cmd([[
" FIXME: C-x C-c is not working actually:
nnoremap <silent> <C-x><C-c> :qa<CR>
nnoremap <silent> <C-x><C-s> :w<CR>
inoremap <silent> <C-s> <ESC>:w<CR>

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
]])

vim.cmd([[
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
]])

-- True color support
vim.opt.termguicolors = true
-- let &t_8f = '\<Esc>[38;2;%lu;%lu;%lum'
-- let &t_8b = '\<Esc>[48;2;%lu;%lu;%lum'

vim.cmd([[
let $SHELL = 'bash'

if executable('fish')
  set shell=fish
endif
]])

vim.opt.number = true

vim.api.nvim_create_user_command("SourceConfig", function()
  -- Run the `tangle` command
  local config = vim.fn.stdpath("config")
  local tangle = vim.fs.joinpath(config, "tangle")
  vim.fn.system(tangle, config)
  -- Reload the buffer in case it's `init.lua`
  vim.cmd("edit!")

  -- Source `init.lua`
  -- vim.cmd("luafile " .. vim.fn.stdpath("config") .. "/init.lua")
  -- Reloading not supported!!
end, {})

vim.cmd([[
function! Abbreviate(from, to)
  exec 'cnoreabbrev <expr> '.a:from
        \ .' ((getcmdtype() ==# ":" && getcmdline() ==# "'.a:from.'")'
        \ .'? ("'.a:to.'") : ("'.a:from.'"))'
endfunction

call Abbreviate('s', 'SourceConfig')
call Abbreviate('ed', 'edit ~/.config/nvim/init.org')

call Abbreviate('h', 'tab help')
call Abbreviate('hs', 'split')
]])

vim.cmd([[
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
]])

require("lazy").setup({

{
  "mattn/vim-tanakh",
  lazy = true,
  config = function()
    vim.opt.statusline = vim.fn["tanakh#face"]()
    vim.api.nvim_create_autocmd({"CursorMoved", "CursorMovedI"}, {
      callback = function()
        vim.opt.statusline = vim.fn["tanakh#face"]()
      end
    })
  end,
},

{
  'nvim-orgmode/orgmode',
  event = 'VeryLazy',
  ft = { 'org' },
  config = function()
    require('orgmode').setup({
      org_startup_folded = 'showeverything',
      -- org_startup_indented = true,
      org_agenda_files = '~/org/**/*',
      org_default_notes_file = '~/org/refile.org',
    })
  end,
},

{
  'nvim-telescope/telescope.nvim',
  tag = '0.1.8',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    local builtin = require('telescope.builtin')
    vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Telescope find files' })
    vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
    vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })
    vim.keymap.set('n', '<leader>gr', builtin.live_grep, { desc = 'Telescope live grep' })

    -- TODO: quit telescope with escape

    require('telescope').setup({
      defaults = {
        mappings = {
          i = {
            ["<C-h>"] = "which_key"
          }
        }
      },
      pickers = {
        -- picker_name = {
        --   picker_config_key = value,
        --   ...
        -- }
      },
      extensions = {
        -- extension_name = {
        --   extension_config_key = value,
        -- }
      }
    })
  end
},

{
  "nvim-orgmode/telescope-orgmode.nvim",
  dependencies = {
    "nvim-telescope/telescope.nvim",
    "nvim-orgmode/orgmode",
  },
  config = function()
    require("telescope").load_extension("orgmode")

    vim.api.nvim_create_autocmd('FileType', {
      pattern = 'org',
      group = vim.api.nvim_create_augroup('orgmode_telescope_nvim', { clear = true }),
      callback = function()
        vim.keymap.set('n', '<leader>or', require('telescope').extensions.orgmode.refile_heading)
      end,
    })

    vim.keymap.set('n', '<leader>Oo', function()
      require('telescope').extensions.orgmode.search_headings()
    end, { desc = 'Telescope orgmode outline' })
  end,
},

{
  "lambdalisue/vim-fern",
  config = function()
    vim.keymap.set('n', '<leader>nn',  '<cmd>Fern . -reveal=% -drawer -right<cr>')
    vim.keymap.set('n', '<leader>nt',  '<cmd>Fern . -reveal=% -drawer -right -toggle<cr>')
  end
},

{
  "lambdalisue/vim-nerdfont",
},

{
  "lambdalisue/vim-fern-renderer-nerdfont",
  dependencies = { 'lambdalisue/vim-fern', 'lambdalisue/vim-nerdfont' },
  config = function()
    vim.g["fern#renderer"] = "nerdfont"
  end
},

{
  "Josiah-tan/plover-vim",
  config = function()
    --
  end
},

{
  "Josiah-tan/plover-vim-tutor",
  dependencies = { 'Josiah-tan/plover-vim' },
  config = function()
    vim.keymap.set('n', '<leader>ap', '<cmd>help plover-vim-tutor<CR>') -- open tape window
  end
},

{
  "derekthecool/plover-tapey-tape.nvim",
  dependencies = { 'derekthecool/plover-tapey-tape.nvim', 'derekthecool/plover-tapey-tape.nvim' },
  config = function()
    vim.keymap.set('n', '<leader>at', require('plover-tapey-tape').toggle) -- open tape window
    vim.keymap.set('n', '<leader>as', require('plover-tapey-tape').stop) -- stop plugin
    require('plover-tapey-tape').setup({
      -- filepath = 'auto',
      -- open_method = 'vsplit',
      -- vertical_split_height = 9,
      -- horizontal_split_width = 54,
      -- steno_capture = '|(.-)|',
      -- suggestion_notifications = {
      --     enabled = true,
      -- },
      -- status_line_setup = {
      --     enabled = true,
      --     additional_filter = '(|.-|)',
      -- },
    })
  end
},

{
  -- 'hukl/Smyck-Color-Scheme',
  'dim13/smyck.vim',
  -- lazy = false,
  config = function()
    vim.cmd("colorscheme smyck")
  end,
},

})
