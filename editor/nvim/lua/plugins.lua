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

    --------------------------------------------------------------------------------
    -- Dependencies
    --------------------------------------------------------------------------------
    use({ 'nvim-lua/popup.nvim' })
    use({ 'nvim-lua/plenary.nvim' })
    use({ 'MunifTanjim/nui.nvim' })
    use({ 'vim-denops/denops.vim' })

    use({ 'rcarriga/nvim-notify' })

    --------------------------------------------------------------------------------
    -- Passive
    --------------------------------------------------------------------------------
    use({ 'tpope/vim-repeat' })
    use({ 'kyazdani42/nvim-web-devicons' })

    use({
        'lambdalisue/glyph-palette.vim',
        config = function()
            vim.cmd([[
                augroup my-glyph-palette
                    autocmd! *
                    autocmd FileType fern call glyph_palette#apply()
                    autocmd FileType nerdtree,startify call glyph_palette#apply()
                augroup END
            ]])
        end,
    })

    use({
        'lambdalisue/nerdfont.vim',
    })

    --------------------------------------------------------------------------------
    -- Operations
    --------------------------------------------------------------------------------
    use({ 'terryma/vim-expand-region' })
    use({ "machakann/vim-sandwich" })

    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    }

    --------------------------------------------------------------------------------
    -- Fuzzy finder
    --------------------------------------------------------------------------------
    use {
        'nvim-telescope/telescope.nvim',
        requires = { {'nvim-lua/plenary.nvim'} }
    }

    use {
       "ahmedkhalf/project.nvim",
       config = function()
           require('project_nvim').setup({
               Update_cwd = true,
               Update_focused_file = {
                   enable = true,
                   update_cwd = true
               },
           })
       end
    }

    --------------------------------------------------------------------------------
    -- Sidebar
    --------------------------------------------------------------------------------
    use {
         'kyazdani42/nvim-tree.lua',
         requires = {'kyazdani42/nvim-web-devicons'},

         -- keys: [a]dd, [d]elete, [r]ename
         config = function()
             require("nvim-tree").setup({
                 update_cwd = true,
                 update_focused_file = {
                     enable = true,
                     update_cwd = true
                 },
                view = {
                    side = "right",
                },
             })
         end,
    }

    --------------------------------------------------------------------------------
    -- Git
    --------------------------------------------------------------------------------
    use({
        'TimUntersberger/neogit',
        requires = {
            'nvim-lua/plenary.nvim',
            'sindrets/diffview.nvim',
        },

        -- opt = true,
        -- cmd = {'Neogit'},

        config = function()
            local neogit = require("neogit")
            neogit.setup {
                integrations = { diffview = true },
            }
        end,
    })

    use {
        'akinsho/git-conflict.nvim',
        config = function()
            require('git-conflict').setup()
        end
    }

    use({ 'lambdalisue/gin.vim' })

    --------------------------------------------------------------------------------
    -- Functions
    --------------------------------------------------------------------------------
    use({ 'famiu/bufdelete.nvim', opt = true, cmd = {'Bdelete', 'Bwipeout'} })
    use({ 'flazz/vim-colorschemes' })

    if packer_bootstrap then
        require('packer').sync()
    end
end)
