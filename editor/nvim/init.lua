-- `init.lua`

-- Define `:ed` and `:s`
vim.cmd([[
function! Abbreviate(from, to)
    exec 'cnoreabbrev <expr> '.a:from
          \ .' ((getcmdtype() ==# ":" && getcmdline() ==# "'.a:from.'")'
          \ .'? ("'.a:to.'") : ("'.a:from.'"))'
endfunction

call Abbreviate('s',  'source ~/dotfiles/editor/nvim/init.lua')
call Abbreviate('ed', 'edit ~/dotfiles/editor/nvim/init.lua')
]])

-- Shared configuration
vim.cmd(
	[[
for file in split(glob('~/.config/nvim/rc/myplugins/*.lua'), '\n')
    execute 'source ' . file
endfor
]])

-- Local configuration
if vim.fn.filereadable(vim.fn.expand("~/.nvim_local_init.lua")) ~= 0 then
    dofile(vim.fn.expand("~/.nvim_local_init.lua"))
end

