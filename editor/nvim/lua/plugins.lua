-- Plugins installed with `Packer`:
-- https://github.com/wbthomason/packer.nvim

-- Bootstrapping of `Packer` is done by cheovim:
-- https://github.com/NTBBloodbath/cheovim

-- local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"
-- if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
	-- vim.api.nvim_command("silent !git clone https://github.com/wbthomason/packer.nvim " .. install_path)
-- end

return require('packer').startup(function(use)
    use({ 'wbthomason/packer.nvim' })

    -- Dependencies
	use({ 'nvim-lua/popup.nvim' })
	use({ 'nvim-lua/plenary.nvim' })
    use({ 'MunifTanjim/nui.nvim' })
    use({ 'vim-denops/denops.vim' })

    use({ 'rcarriga/nvim-notify' })

    -- Passive
    use({ 'tpope/vim-repeat' })
    use({ 'kyazdani42/nvim-web-devicons' })

    -- Operations
    use({ 'terryma/vim-expand-region' })
    use({ "machakann/vim-sandwich" })

    -- Widgets
    use({ 'lambdalisue/gin.vim' })

    -- Functions
    use({ 'famiu/bufdelete.nvim', opt = true, cmd = {'Bdelete', 'Bwipeout'} })
    use({ 'flazz/vim-colorschemes' })

    -- if packer_bootstrap then
    --     require('packer').sync()
    -- end
end)
