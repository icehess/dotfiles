-- Automatically install packer
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)


local opts = {
  install = {
    missing = true,
    colorscheme = { "rasmus", "habamax", "lunar" },
  },
  ui = {
    border = "rounded",
  },
  git = {
    timeout = 120,
  },
  performance = {
    rtp = {
      reset = false,
    },
  }
}

local plugins = {
  -- Have package manager manage itself
  { "folke/lazy.nvim" },
  -- { "dstein64/vim-startuptime" },

  -- Useful lua functions used by lots of plugins
  { "nvim-lua/plenary.nvim" },
  { "nvim-lua/popup.nvim" },

  -- Telescope
  {
    "nvim-telescope/telescope.nvim",
    config = function()
      require("hesaam.core.telescope").setup()
    end,
    lazy = true,
    event = "VeryLazy",
    cmd = "Telescope",
    dependencies = {
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
      }
    }
  },

  -- Theme
  { 'rose-pine/neovim', name = 'rose-pine' },
  { 'cocopon/iceberg.vim' },
  { 'kvrohit/rasmus.nvim' },
  { 'rockyzhang24/arctic.nvim', dependencies = { { "rktjmp/lush.nvim" } } },

  -- Treesitter
  { 'nvim-treesitter/nvim-treesitter' },
  { 'nvim-treesitter/playground', lazy = true, event = "VeryLazy", },

  -- Whichkey
  {
    "folke/which-key.nvim",
    lazy = true,
    event = "VeryLazy",
    config = function()
      require("hesaam.core.which-key").setup()
    end,
  },

  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("hesaam.core.gitsigns").setup()
    end,
    event = "BufRead",
  },

  {
    'VonHeikemen/lsp-zero.nvim',
    lazy = true,
    event = "VeryLazy",
    config = function()
      require("hesaam.core.lsp").setup()
    end,
    dependencies = {
      -- LSP Support
      {
        'neovim/nvim-lspconfig',
      },
      {
        'williamboman/mason.nvim',
        config = function()
          require("hesaam.core.mason").setup()
        end,
      },
      { 'williamboman/mason-lspconfig.nvim' },

      -- for formatters and linters
      { "jose-elias-alvarez/null-ls.nvim" },

      -- Autocompletion
      { 'hrsh7th/nvim-cmp' },
      { 'hrsh7th/cmp-buffer' },
      { 'hrsh7th/cmp-path' },
      { 'saadparwaiz1/cmp_luasnip' },
      { 'hrsh7th/cmp-nvim-lsp' },
      { 'hrsh7th/cmp-nvim-lua' },

      -- Snippets
      { 'L3MON4D3/LuaSnip' },
      { 'rafamadriz/friendly-snippets' },
    }
  },

  {
    "nvim-tree/nvim-tree.lua",
    -- event = "BufWinOpen",
    -- cmd = "NvimTreeToggle",
    config = function()
      require("hesaam.core.nvimtree").setup()
    end,
  },

  { "JoosepAlviste/nvim-ts-context-commentstring", event = "VeryLazy" },
  {
    "numToStr/Comment.nvim",
    event = "BufRead",
    config = function()
      require("hesaam.core.comment").setup()
    end,
  },

  {
    "nvim-lualine/lualine.nvim",
    config = function()
      require("hesaam.core.lualine").setup()
    end,
  },

  {
    "akinsho/bufferline.nvim",
    config = function()
      require("hesaam.core.bufferline").setup()
    end,
    branch = "main",
    enabled = true,
  },
  { 'kdheepak/lazygit.nvim', lazy=true, event="VeryLazy" },
  { 'tpope/vim-fugitive', lazy=true, event="VeryLazy" },
  { "nvim-tree/nvim-web-devicons" },
}
require("lazy").setup(plugins, opts)
