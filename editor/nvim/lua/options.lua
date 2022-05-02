-- Options

local opt = vim.opt

--------------------------------------------------------------------------------
-- Color
--------------------------------------------------------------------------------

-- nvim color
vim.env.NVIM_TUI_ENABLE_TRUE_COLOR = 1
-- vim.o.t_Co = "256"

-- -- True color support
-- vim.g.colorterm = os.getenv("COLORTERM")
-- if
--     vim.g.colorterm == "truecolor"
--     or vim.g.colorterm == "24bit"
--     or vim.g.colorterm == "rxvt"
--     or vim.g.colorterm == ""
-- then
--     if vim.fn.exists("+termguicolors") then
--         vim.o.t_8f = "<Esc>[38;2;%lu;%lu;%lum"
--         vim.o.t_8b = "<Esc>[48;2;%lu;%lu;%lum"
--         vim.o.termguicolors = true
--     end
-- end

--------------------------------------------------------------------------------
-- System
--------------------------------------------------------------------------------

-- Non-interactive shell:
vim.env.SHELL = 'bash'

-- Interactive shell:
if vim.fn.executable('fish') == 1 then
    opt.shell = 'fish'
end

-- TODO: update NeoVim
-- opt.shellslash = true    -- Allow `/` on Windows for path

-- TODO:
-- language C               -- English as the system language
vim.cmd([[ syntax enable ]]) -- Allow builting highlight
-- filetype plugin on       -- I forgot what this is
opt.updatetime = 300     -- Make vim faster
opt.timeoutlen = 500     -- For which-key
opt.synmaxcol = 200      -- For too long lines

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

-- TODO: what?
-- opt.title = false

-- top
opt.showtabline=2        --  Show tab (even when no such plugin is enabled)

-- left
opt.number = true        -- Show line numbers

-- bottom
opt.showcmd = true       -- Show input at right bottom coner
opt.ruler = true         -- Show the line and column number of the cursor position
opt.showmatch = true     -- Show matching pair of symbols

-------------------------------------------------------------------------------
-- Input
-------------------------------------------------------------------------------

opt.belloff = 'all'      -- No sound
opt.mouse = 'nv'         -- Mouse input enabled
opt.backspace = "2"      -- Backspace enabled in insert mode

--------------------------------------------------------------------------------
-- File
--------------------------------------------------------------------------------

-- encoding
opt.encoding= "utf-8"    -- Use UTF-8
-- vim.o.fileencodings = "ucs-bom,utf-8,euc-jp,iso-2022-jp,cp932,sjis,latin1"
-- vim.o.fileformats = "unix,dos,mac"

-- Auto files
opt.swapfile = false     -- Do not create unnecessary files
opt.backup = false       -- Do not create unnecessary files
-- opt.backupdir = vim.fn.stdpath("data") .. "/backup/"
opt.undofile = false     -- Do not create unnecessary files

-- Buffers
opt.hidden = true        -- Allow switching buffers even if it's not yet saved
opt.autoread = true      --  Reload when the file is modified

vim.cmd('autocmd FocusGained,BufEnter * :checktime') -- Detect external changes
vim.cmd('autocmd FocusGained,BufEnter * :redraw!')   -- and automatically refresh screen

--------------------------------------------------------------------------------
-- Coding
--------------------------------------------------------------------------------

-- search
opt.incsearch = true     -- Incremental serach (ESC or C-[ to cancel)
opt.ignorecase = true    -- Ignoring up/down case
opt.smartcase = true     -- Only down case ignores up/down
opt.hlsearch = true      -- Highlight matches
opt.wrapscan = true      -- End -> Start

-- indent
opt.autoindent = true
opt.smartindent = true
opt.list = true
opt.listchars = {tab = '>-', trail = '*', nbsp = '+'}

opt.foldmethod="indent"
opt.foldlevel=99

--------------------------------------------------------------------------------
-- Preferences
--------------------------------------------------------------------------------

opt.expandtab = true
opt.tabstop = 4
opt.shiftwidth = 4

-- TODO:
-- opt.display += "lastline"

-- TODO:
-- opt.fillchars=fo ld:\  " do not show `...` characters

opt.scrolloff=3          -- Margins for scrolling
opt.startofline = false  -- Keep cursor position on screen with `C-f` or `C-b`

-- augroup UseTab
--     autocmd FileType make set noexpandtab
-- augroup END

-- augroup SmarterNavigation
--     autocmd!
--     autocmd TermOpen * set nobuflisted
--     autocmd BufWinEnter quickfix set nobuflisted
-- augroup END

