-- `init.lua`

-- Define `:ed` and `:s`
vim.cmd('source ~/dotfiles/editor/nvim/keymap.vim')
vim.cmd('source ~/dotfiles/editor/nvim/setup.vim')

-- Load the shared configurations
vim.cmd([[
for file in split(glob('~/dotfiles/editor/nvim/lua/*.lua'), '\n')
    execute 'source ' . file
endfor
]])

-- Load the local configuration
if vim.fn.filereadable(vim.fn.expand("~/.nvim_local_init.lua")) ~= 0 then
    dofile(vim.fn.expand("~/.nvim_local_init.lua"))
end

-- Call `setup.nvim` functions
vim.cmd('call AfterInit()')

