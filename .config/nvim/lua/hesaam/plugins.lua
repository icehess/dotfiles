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

  { "ibhagwan/fzf-lua",         lazy = true },
  {
    "junegunn/fzf.vim",
    lazy = true,
    event = "VeryLazy",
    dependencies = {
      {
        "junegunn/fzf",
        -- config = function()
        --   vim.exec("fzf#install()")
        -- end
      },
    }
  },

  -- Theme
  { 'rose-pine/neovim',         name = 'rose-pine' },
  { 'cocopon/iceberg.vim' },
  { 'kvrohit/rasmus.nvim' },
  { 'rockyzhang24/arctic.nvim', dependencies = { { "rktjmp/lush.nvim" } } },
  { 'kyazdani42/blue-moon' },
  { 'sainnhe/gruvbox-material' },

  -- Treesitter
  {
    'nvim-treesitter/nvim-treesitter',
    config = function()
      require("hesaam.core.treesitter").setup()
    end
  },
  { 'nvim-treesitter/playground',                  lazy = true,       event = "VeryLazy", },

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
    branch = 'v2.x',
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
      { 'hrsh7th/cmp-nvim-lsp' },
      { 'hrsh7th/cmp-buffer' },
      { 'hrsh7th/cmp-path' },
      { 'saadparwaiz1/cmp_luasnip' },
      { 'hrsh7th/cmp-nvim-lua' },

      -- Snippets
      { 'L3MON4D3/LuaSnip' },
      { 'rafamadriz/friendly-snippets' },
    }
  },

  -- Debugging
  {
    "mfussenegger/nvim-dap",
    -- event = "BufWinEnter",
    lazy = true,
    event = "VeryLazy",
    config = function()
      require("hesaam.core.dap").setup()
    end,
  },

  -- Debugger user interface
  {
    "rcarriga/nvim-dap-ui",
    lazy = true,
    event = "VeryLazy",
    config = function()
      require("hesaam.core.dap").setup_ui()
    end,
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
  { 'kdheepak/lazygit.nvim',      lazy = true, event = "VeryLazy" },
  { 'tpope/vim-fugitive',         lazy = true, event = "VeryLazy" },
  { "nvim-tree/nvim-web-devicons" },

  -- {
  --   "RRethy/vim-illuminate",
  --   config = function()
  --     require("hesaam.core.illuminate").setup()
  --   end,
  --   event = "VeryLazy",
  -- },

  -- {
  --   "lukas-reineke/indent-blankline.nvim",
  --   config = function()
  --     require("hesaam.core.indentlines").setup()
  --   end,
  -- },

  {
    -- other options:
    -- * https://github.com/NMAC427/guess-indent.nvim
    "Darazaki/indent-o-matic",
    lazy = true,
    event = "VeryLazy",
    confg = function()
      require("hesaam.core.indent-o-matic").setup()
    end
  },

    -- {
    --   -- other options:
    --   -- * https://github.com/NMAC427/guess-indent.nvim
    --   "Darazaki/indent-o-matic",
    --   lazy = true,
    --   event = "VeryLazy",
    --   confg = function()
    --     require("hesaam.core.indent-o-matic").setup()
    --   end
    -- },

    {
      'nmac427/guess-indent.nvim',
         config = function() require('guess-indent').setup {} end,
     },
  -- This plugin disables certain features if the opened file is big. File size and features to disable are configurable.
  -- Automatic features/integrations include: LSP, treesitter, indent_blankline, illuminate.vim NoMatchParen, syntax off, ... (full list at the end)
  {
    "lunarvim/bigfile.nvim",
    -- config = function()
    --   pcall(function()
    --     require("bigfile").config(hesaam.bigfile.config)
    --   end)
    -- end,
  },

  -- winbar that shows file path and LSP context
  {
    "utilyre/barbecue.nvim",
    name = "barbecue",
    lazy = true,
    event = "VeryLazy",
    dependencies = {
      "SmiteshP/nvim-navic",
    },
    opts = {
      -- configurations go here
      show_navic = true,
    },
  },

  {
    'andymass/vim-matchup',
    config = function()
      -- may set any options here
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
    end
  },

  { 'duane9/nvim-rg' },

  {
    'NvChad/nvim-colorizer.lua',
    config = function()
      require("colorizer").setup {
        filetypes = {
          'css',
          'javascript',
          'typescript',
          'html'
        },
        user_default_options = {
          RGB = true, -- #RGB hex codes
          RRGGBB = true, -- #RRGGBB hex codes
          names = true, -- "Name" codes like Blue or blue
          RRGGBBAA = true, -- #RRGGBBAA hex codes
          AARRGGBB = true, -- 0xAARRGGBB hex codes
          rgb_fn = true, -- CSS rgb() and rgba() functions
          hsl_fn = true, -- CSS hsl() and hsla() functions
          css = true, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
          css_fn = true, -- Enable all CSS *functions*: rgb_fn, hsl_fn
          -- Available modes for `mode`: foreground, background,  virtualtext
          mode = "background", -- Set the display mode.
          -- Available methods are false / true / "normal" / "lsp" / "both"
          -- True is same as normal
          tailwind = true, -- Enable tailwind colors
          -- parsers can contain values used in |user_default_options|
          sass = { enable = true, parsers = { "css" }, }, -- Enable sass colors
          virtualtext = "■",
          -- update color values even if buffer is not focused
          -- example use: cmp_menu, cmp_docs
          always_update = false
        },
        -- all the sub-options of filetypes apply to buftypes
        buftypes = {},
      }
    end
  },
}
require("lazy").setup(plugins, opts)
