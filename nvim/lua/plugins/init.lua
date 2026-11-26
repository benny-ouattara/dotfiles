local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

return require("lazy").setup(
{
  "rafamadriz/friendly-snippets",
  "saadparwaiz1/cmp_luasnip",
  "hrsh7th/cmp-nvim-lua",
  "hrsh7th/cmp-nvim-lsp",
  "hrsh7th/cmp-path",
  "junegunn/vim-easy-align",
  {
    -- The `main` branch is a from-scratch rewrite: it only installs parsers and
    -- ships queries, Neovim owns the features. It cannot be lazy-loaded.
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    lazy = false,
    build = ":TSUpdate",
    config = function()
      require "plugins.treesitter"
    end,
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      require "plugins.lspconfig"
    end,
  },
  {
    -- mason moved to the mason-org organisation.
    "mason-org/mason.nvim",
    config = function()
      require "plugins.mason"
    end,
  },
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = { "mason-org/mason.nvim", "neovim/nvim-lspconfig" },
    config = function()
      require "plugins.mason_lspconfig"
    end,
  },
  {
    "hrsh7th/nvim-cmp",
    config = function()
      require "plugins.cmp"
    end,
  },
  {
    "L3MON4D3/LuaSnip",
    dependencies = "onsails/lspkind.nvim",
    build = "make install_jsregexp",
    config = function()
      require "plugins.luasnip"
    end,
  },
  {
    "ray-x/lsp_signature.nvim",
    config = function()
      require "lsp_signature".setup()
    end
  },
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require "gitsigns".setup()
    end
  },
  {
    lazy = true,
    "mfussenegger/nvim-dap",
    config = function()
      require "plugins.dap"
    end,
  },
  {
    event = "BufEnter *.py",
    "mfussenegger/nvim-dap-python",
    config = function()
      require "dap-python".setup("python")
    end,
  },
  {
    lazy = true,
    "rcarriga/nvim-dap-ui",
    dependencies = "nvim-neotest/nvim-nio",
    config = function()
      require "plugins.dapui"
    end,
  },
  {
    -- norcalli's original is dormant; this fork is the maintained continuation.
    "catgoose/nvim-colorizer.lua",
    config = function()
      require "colorizer".setup()
    end
  },
  {
    "akinsho/bufferline.nvim",
    dependencies = "nvim-tree/nvim-web-devicons",
    config = function()
      require "plugins.bufferline"
    end
  },
  {
    "folke/todo-comments.nvim",
    dependencies = "nvim-lua/plenary.nvim",
    config = function()
      require "todo-comments".setup()
    end
  },
  {
    -- Development moved off GitHub; the GitHub repo is now an empty stub.
    -- Leap lazy-loads itself, so don't let lazy.nvim defer it.
    "leap.nvim",
    url = "https://codeberg.org/andyg/leap.nvim",
    lazy = false,
  },
  {
    cmd = "Telescope",
    "nvim-telescope/telescope.nvim",
    dependencies = "nvim-lua/plenary.nvim",
    config = function()
      require("telescope").setup()
    end
  },
  {
    lazy = true,
    "LeonHeidelbach/trailblazer.nvim",
    config = function()
        require("plugins.trailblazer").init()
    end
  },
  {
    lazy = true,
    "chrisgrieser/nvim-spider"
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require "plugins.blankline"
    end,
  },
  {
    priority = 1000,
    "catppuccin/nvim",
    name = "catppuccin",
    build = ":CatppuccinCompile",
    config = function()
      require "plugins.catppuccin"
    end
  },
})
