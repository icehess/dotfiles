local M = {}

M.config = {
  config = {
    plugins = {
      marks = true, -- shows a list of your marks on ' and `
      registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
      spelling = {
        enabled = true,
        suggestions = 20,
      }, -- use which-key for spelling hints
      -- the presets plugin, adds help for a bunch of default keybindings in Neovim
      -- No actual key bindings are created
      presets = {
        operators = true, -- adds help for operators like d, y, ...
        motions = true, -- adds help for motions
        text_objects = true, -- help for text objects triggered after entering an operator
        windows = true, -- default bindings on <c-w>
        nav = true, -- misc bindings to work with windows
        z = true, -- bindings for folds, spelling and others prefixed with z
        g = true, -- bindings for prefixed with g
      },
    },
    -- add operators that will trigger motion and text object completion
    -- to enable all native operators, set the preset / operators plugin above
    operators = { gc = "Comments" },
    key_labels = {
      -- override the label used to display some keys. It doesn't effect WK in any other way.
      -- For example:
      -- ["<space>"] = "SPC",
      -- ["<cr>"] = "RET",
      -- ["<tab>"] = "TAB",
    },
    icons = {
      breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
      separator = "➜", -- symbol used between a key and it's label
      group = "+", -- symbol prepended to a group
    },
    popup_mappings = {
      scroll_down = "<c-d>", -- binding to scroll down inside the popup
      scroll_up = "<c-u>", -- binding to scroll up inside the popup
    },
    window = {
      border = "single", -- none, single, double, shadow
      position = "bottom", -- bottom, top
      margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
      padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
      winblend = 0,
    },
    layout = {
      height = { min = 4, max = 25 }, -- min and max height of the columns
      width = { min = 20, max = 50 }, -- min and max width of the columns
      spacing = 3, -- spacing between columns
      align = "left", -- align columns left, center or right
    },
    ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
    -- hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
    show_help = true, -- show help message on the command line when the popup is visible
    show_keys = true, -- show the currently pressed key and its label as a message in the command line
    triggers = "auto", -- automatically setup triggers
    -- triggers = {"<leader>"} -- or specify a list manually
    triggers_blacklist = {
      -- list of mode / prefixes that should never be hooked by WhichKey
      -- this is mostly relevant for key maps that start with a native binding
      -- most people should not need to change this
      i = { "j", "k" },
      v = { "j", "k" },
    },
    -- disable the WhichKey popup for certain buf types and file types.
    -- Disabled by deafult for Telescope
    disable = {
      buftypes = {},
      filetypes = { "TelescopePrompt" },
    },
  },
  normal_leader_opts = {
    mode = "n", -- NORMAL mode
    prefix = "<leader>",
    buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
    silent = true, -- use `silent` when creating keymaps
    noremap = true, -- use `noremap` when creating keymaps
    nowait = true, -- use `nowait` when creating keymaps
  },

  visual_leader_opts = {
    mode = "v", -- VISUAL mode
    prefix = "<leader>",
    buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
    silent = true, -- use `silent` when creating keymaps
    noremap = true, -- use `noremap` when creating keymaps
    nowait = true, -- use `nowait` when creating keymaps
  },
  -- NOTE: Prefer using : over <cmd> as the latter avoids going back in normal-mode.
  -- see https://neovim.io/doc/user/map.html#:map-cmd
  visual_leader_mappings = {
    ["/"] = { "<Plug>(comment_toggle_linewise_visual)", "Comment toggle linewise (visual)" },
  },

  normal_leader_mappings = {
    { "<leader>/", "<Plug>(comment_toggle_linewise_current)", desc = "Comment toggle current line", nowait = true, remap = false },
    { "<leader>;", "<cmd>Alpha<CR>", desc = "Dashboard", nowait = true, remap = false },
    { "<leader>F", "<cmd>Files<cr>", desc = "Fzf Files", nowait = true, remap = false },
    { "<leader>T", group = "Treesitter", nowait = true, remap = false },
    { "<leader>Ti", ":TSConfigInfo<cr>", desc = "Info", nowait = true, remap = false },
    { "<leader>\\", group = "Useful Keymaps", nowait = true, remap = false },
    { "<leader>\\A", "<cmd>set noexpandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>", desc = "Use Tab instead of space", nowait = true, remap = false },
    { "<leader>\\g", "<cmd>Gitsigns toggle_signs<cr>", desc = "Toggle Gitsigns", nowait = true, remap = false },
    { "<leader>\\n", "<cmd>setlocal number!<cr>:setlocal number?<cr>", desc = "Toggle line number", nowait = true, remap = false },
    { "<leader>\\s", "<cmd>setlocal invspell<CR>", desc = "Highlight spell Check", nowait = true, remap = false },
    { "<leader>b", group = "Buffers", nowait = true, remap = false },
    { "<leader>bD", "<cmd>BufferLineSortByDirectory<cr>", desc = "Sort by directory", nowait = true, remap = false },
    { "<leader>bL", "<cmd>BufferLineSortByExtension<cr>", desc = "Sort by language", nowait = true, remap = false },
    { "<leader>bS", "<cmd>lua require('telescope.builtin').grep_string({grep_open_files = true})<cr>", desc = "Grep current string in open files", nowait = true, remap = false },
    { "<leader>bW", "<cmd>noautocmd w<cr>", desc = "Save without formatting (noautocmd)", nowait = true, remap = false },
    { "<leader>bb", "<cmd>BufferLineCyclePrev<cr>", desc = "Previous", nowait = true, remap = false },
    { "<leader>be", "<cmd>BufferLinePickClose<cr>", desc = "Pick which buffer to close", nowait = true, remap = false },
    { "<leader>bf", "<cmd>Telescope buffers previewer=false<cr>", desc = "Find", nowait = true, remap = false },
    { "<leader>bh", "<cmd>BufferLineCloseLeft<cr>", desc = "Close all to the left", nowait = true, remap = false },
    { "<leader>bj", "<cmd>BufferLinePick<cr>", desc = "Jump", nowait = true, remap = false },
    { "<leader>bl", "<cmd>BufferLineCloseRight<cr>", desc = "Close all to the right", nowait = true, remap = false },
    { "<leader>bn", "<cmd>BufferLineCycleNext<cr>", desc = "Next", nowait = true, remap = false },
    { "<leader>bs", "<cmd>lua require('telescope.builtin').live_grep({grep_open_files = true})<cr>", desc = "Grep Open Files", nowait = true, remap = false },
    { "<leader>c", "<cmd>lua require('hesaam.core.bufferline').buf_kill 'bd'<CR>", desc = "Close Buffer", nowait = true, remap = false },
    { "<leader>d", group = "Debug", nowait = true, remap = false },
    { "<leader>dC", "<cmd>lua require'dap'.run_to_cursor()<cr>", desc = "Run To Cursor", nowait = true, remap = false },
    { "<leader>dU", "<cmd>lua require'dapui'.toggle({reset = true})<cr>", desc = "Toggle UI", nowait = true, remap = false },
    { "<leader>db", "<cmd>lua require'dap'.step_back()<cr>", desc = "Step Back", nowait = true, remap = false },
    { "<leader>dc", "<cmd>lua require'dap'.continue()<cr>", desc = "Continue", nowait = true, remap = false },
    { "<leader>dd", "<cmd>lua require'dap'.disconnect()<cr>", desc = "Disconnect", nowait = true, remap = false },
    { "<leader>dg", "<cmd>lua require'dap'.session()<cr>", desc = "Get Session", nowait = true, remap = false },
    { "<leader>di", "<cmd>lua require'dap'.step_into()<cr>", desc = "Step Into", nowait = true, remap = false },
    { "<leader>do", "<cmd>lua require'dap'.step_over()<cr>", desc = "Step Over", nowait = true, remap = false },
    { "<leader>dp", "<cmd>lua require'dap'.pause()<cr>", desc = "Pause", nowait = true, remap = false },
    { "<leader>dq", "<cmd>lua require'dap'.close()<cr>", desc = "Quit", nowait = true, remap = false },
    { "<leader>dr", "<cmd>lua require'dap'.repl.toggle()<cr>", desc = "Toggle Repl", nowait = true, remap = false },
    { "<leader>ds", "<cmd>lua require'dap'.continue()<cr>", desc = "Start", nowait = true, remap = false },
    { "<leader>dt", "<cmd>lua require'dap'.toggle_breakpoint()<cr>", desc = "Toggle Breakpoint", nowait = true, remap = false },
    { "<leader>du", "<cmd>lua require'dap'.step_out()<cr>", desc = "Step Out", nowait = true, remap = false },
    { "<leader>e", "<cmd>NvimTreeToggle<CR>", desc = "Explorer", nowait = true, remap = false },
    { "<leader>f", "<cmd>Telescope find_files<CR>", desc = "Telescope Find File", nowait = true, remap = false },
    { "<leader>g", group = "Git", nowait = true, remap = false },
    { "<leader>gC", "<cmd>lua require('telescope.builtin').git_bcommits({cmd=vim.fn.expand('%:p:h')})<cr>", desc = "Commits(for current file)", nowait = true, remap = false },
    { "<leader>gb", "<cmd>lua require('telescope.builtin').git_branches({cmd=vim.fn.expand('%:p:h')})<cr>", desc = "Branches", nowait = true, remap = false },
    { "<leader>gc", "<cmd>lua require('telescope.builtin').git_commits({cmd=vim.fn.expand('%:p:h')})<cr>", desc = "Commits", nowait = true, remap = false },
    { "<leader>gd", "<cmd>Gitsigns diffthis HEAD<cr>", desc = "Git Diff", nowait = true, remap = false },
    { "<leader>gg", "<cmd>G<cr>", desc = "Fugitive", nowait = true, remap = false },
    { "<leader>gh", group = "Hunk", nowait = true, remap = false },
    { "<leader>ghR", "<cmd>lua require 'gitsigns'.reset_buffer()<cr>", desc = "Reset Buffer", nowait = true, remap = false },
    { "<leader>ghj", "<cmd>lua require 'gitsigns'.next_hunk({navigation_message = false})<cr>", desc = "Next Hunk", nowait = true, remap = false },
    { "<leader>ghk", "<cmd>lua require 'gitsigns'.prev_hunk({navigation_message = false})<cr>", desc = "Prev Hunk", nowait = true, remap = false },
    { "<leader>ghp", "<cmd>lua require 'gitsigns'.preview_hunk()<cr>", desc = "Preview Hunk", nowait = true, remap = false },
    { "<leader>ghr", "<cmd>lua require 'gitsigns'.reset_hunk()<cr>", desc = "Reset Hunk", nowait = true, remap = false },
    { "<leader>ghs", "<cmd>lua require 'gitsigns'.stage_hunk()<cr>", desc = "Stage Hunk", nowait = true, remap = false },
    { "<leader>ghu", "<cmd>lua require 'gitsigns'.undo_stage_hunk()<cr>", desc = "Undo Stage Hunk", nowait = true, remap = false },
    { "<leader>gl", "<cmd>lua require 'gitsigns'.blame_line()<cr>", desc = "Blame Line", nowait = true, remap = false },
    { "<leader>go", "<cmd>lua require('telescope.builtin').git_status({cmd=vim.fn.expand('%:p:h')})<cr>", desc = "Status", nowait = true, remap = false },
    { "<leader>gz", "<cmd>LazyGitCurrentFile<cr>", desc = "Lazygit", nowait = true, remap = false },
    { "<leader>h", "<cmd>nohlsearch<CR>", desc = "No Highlight", nowait = true, remap = false },
    { "<leader>l", group = "LSP", nowait = true, remap = false },
    { "<leader>lI", "<cmd>Mason<cr>", desc = "Mason Info", nowait = true, remap = false },
    { "<leader>lS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", desc = "Workspace Symbols", nowait = true, remap = false },
    { "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<cr>", desc = "Code Action", nowait = true, remap = false },
    { "<leader>lb", "<cmd>Telescope diagnostics bufnr=0 theme=get_ivy<cr>", desc = "Buffer Diagnostics", nowait = true, remap = false },
    { "<leader>lc", vim.lsp.codelens.run, desc = "CodeLens Action", nowait = true, remap = false },
    { "<leader>ld", group = "Diagnostics", nowait = true, remap = false },
    { "<leader>ldh", "<cmd>lua vim.diagnostic.hide()<CR>lua vim.diagnostic.config({ signs = false, underline = false })<cr>", desc = "Hide LSP Diagnostics", nowait = true, remap = false },
    { "<leader>lds", "<cmd>lua vim.diagnostic.show()<CR>lua vim.diagnostic.config({ signs = true, underline = true })<cr>", desc = "Show LSP Diagnostics", nowait = true, remap = false },
    { "<leader>le", "<cmd>Telescope quickfix<cr>", desc = "Telescope Quickfix", nowait = true, remap = false },
    { "<leader>lf", vim.lsp.buf.format, desc = "Format", nowait = true, remap = false },
    { "<leader>li", "<cmd>LspInfo<cr>", desc = "Info", nowait = true, remap = false },
    { "<leader>lj", vim.diagnostic.goto_next, desc = "Next Diagnostic", nowait = true, remap = false },
    { "<leader>lk", vim.diagnostic.goto_prev, desc = "Prev Diagnostic", nowait = true, remap = false },
    { "<leader>ll", "<cmd>lua vim.diagnostic.open_float()<cr>", desc = "Show Line Diagnostics", nowait = true, remap = false },
    { "<leader>lq", vim.diagnostic.setloclist, desc = "Quickfix", nowait = true, remap = false },
    { "<leader>lr", vim.lsp.buf.rename, desc = "Rename", nowait = true, remap = false },
    { "<leader>ls", "<cmd>Telescope lsp_document_symbols<cr>", desc = "Document Symbols", nowait = true, remap = false },
    { "<leader>lw", "<cmd>Telescope diagnostics<cr>", desc = "Diagnostics", nowait = true, remap = false },
    { "<leader>p", group = "Plugins", nowait = true, remap = false },
    { "<leader>pS", "<cmd>Lazy clear<cr>", desc = "Status", nowait = true, remap = false },
    { "<leader>pc", "<cmd>Lazy clean<cr>", desc = "Clean", nowait = true, remap = false },
    { "<leader>pd", "<cmd>Lazy debug<cr>", desc = "Debug", nowait = true, remap = false },
    { "<leader>pi", "<cmd>Lazy install<cr>", desc = "Install", nowait = true, remap = false },
    { "<leader>pl", "<cmd>Lazy log<cr>", desc = "Log", nowait = true, remap = false },
    { "<leader>pp", "<cmd>Lazy profile<cr>", desc = "Profile", nowait = true, remap = false },
    { "<leader>ps", "<cmd>Lazy sync<cr>", desc = "Sync", nowait = true, remap = false },
    { "<leader>pu", "<cmd>Lazy update<cr>", desc = "Update", nowait = true, remap = false },
    { "<leader>q", "<cmd>confirm q<CR>", desc = "Quit", nowait = true, remap = false },
    { "<leader>s", group = "Search", nowait = true, remap = false },
    { "<leader>sB", "<cmd>lua require('telescope.builtin').grep_string({grep_open_files = true})<cr>", desc = "Grep current string in open files", nowait = true, remap = false },
    { "<leader>sC", "<cmd>Telescope commands<cr>", desc = "Commands", nowait = true, remap = false },
    { "<leader>sF", "<cmd>Telescope find_files<cr>", desc = "Telescope Show File", nowait = true, remap = false },
    { "<leader>sG", "<cmd>Telescope grep_string<cr>", desc = "Grep current string", nowait = true, remap = false },
    { "<leader>sH", "<cmd>Telescope highlights<cr>", desc = "Find highlight groups", nowait = true, remap = false },
    { "<leader>sM", "<cmd>Telescope man_pages<cr>", desc = "Man Pages", nowait = true, remap = false },
    { "<leader>sR", "<cmd>Telescope registers<cr>", desc = "Registers", nowait = true, remap = false },
    { "<leader>sS", "<cmd>Telescope live_grep<cr>", desc = "Live Grep", nowait = true, remap = false },
    { "<leader>sT", "<cmd>Telescope filetypes<cr>", desc = "Set filetype", nowait = true, remap = false },
    { "<leader>sb", "<cmd>lua require('telescope.builtin').live_grep({grep_open_files = true})<cr>", desc = "Grep Open Files", nowait = true, remap = false },
    { "<leader>sc", "<cmd>lua require('telescope.builtin').colorscheme({enable_preview = true})<cr>", desc = "Colorscheme", nowait = true, remap = false },
    { "<leader>sf", "<cmd>lua require('fzf-lua').files()<cr>", desc = "Fzf.Lua Files", nowait = true, remap = false },
    { "<leader>sg", "<cmd>Telescope grep_string<cr>", desc = "Grep current string", nowait = true, remap = false },
    { "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "Find Help", nowait = true, remap = false },
    { "<leader>sk", "<cmd>Telescope keymaps<cr>", desc = "Keymaps", nowait = true, remap = false },
    { "<leader>sm", "<cmd>Telescope marks<cr>", desc = "Marks", nowait = true, remap = false },
    { "<leader>sr", "<cmd>Telescope oldfiles<cr>", desc = "Recent File", nowait = true, remap = false },
    { "<leader>ss", "<cmd>Telescope live_grep<cr>", desc = "Live Grep", nowait = true, remap = false },
    { "<leader>w", "<cmd>w!<CR>", desc = "Save", nowait = true, remap = false },
  },
  -- {
  --   [";"] = { "<cmd>Alpha<CR>", "Dashboard" },
  --   ["w"] = { "<cmd>w!<CR>", "Save" },
  --   ["q"] = { "<cmd>confirm q<CR>", "Quit" },
  --   ["/"] = { "<Plug>(comment_toggle_linewise_current)", "Comment toggle current line" },
  --   ["c"] = { "<cmd>lua require('hesaam.core.bufferline').buf_kill 'bd'<CR>", "Close Buffer" },
  --   ["f"] = { "<cmd>Telescope find_files<CR>", "Telescope Find File" },
  --   ["F"] = { "<cmd>Files<cr>", "Fzf Files" },
  --   ["h"] = { "<cmd>nohlsearch<CR>", "No Highlight" },
  --   ["e"] = { "<cmd>NvimTreeToggle<CR>", "Explorer" },
  --   ["\\"] = {
  --     name = "Useful Keymaps",
  --     A = { "<cmd>set noexpandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>", "Use Tab instead of space" },
  --     s = { "<cmd>setlocal invspell<CR>", "Highlight spell Check" },
  --     g = { "<cmd>Gitsigns toggle_signs<cr>", "Toggle Gitsigns" },
  --     n = { "<cmd>setlocal number!<cr>:setlocal number?<cr>", "Toggle line number" },
  --   },
  --   b = {
  --     name = "Buffers",
  --     j = { "<cmd>BufferLinePick<cr>", "Jump" },
  --     -- j = { "<cmd>Telescope buffers<cr>", "Jump" },
  --     f = { "<cmd>Telescope buffers previewer=false<cr>", "Find" },
  --     b = { "<cmd>BufferLineCyclePrev<cr>", "Previous" },
  --     n = { "<cmd>BufferLineCycleNext<cr>", "Next" },
  --     W = { "<cmd>noautocmd w<cr>", "Save without formatting (noautocmd)" },
  --     -- w = { "<cmd>BufferWipeout<cr>", "Wipeout" }, -- TODO: implement this for bufferline
  --     e = { "<cmd>BufferLinePickClose<cr>", "Pick which buffer to close" },
  --     h = { "<cmd>BufferLineCloseLeft<cr>", "Close all to the left" },
  --     l = { "<cmd>BufferLineCloseRight<cr>", "Close all to the right" },
  --     D = { "<cmd>BufferLineSortByDirectory<cr>", "Sort by directory" },
  --     L = { "<cmd>BufferLineSortByExtension<cr>", "Sort by language" },
  --     s = { "<cmd>lua require('telescope.builtin').live_grep({grep_open_files = true})<cr>", "Grep Open Files" },
  --     S = { "<cmd>lua require('telescope.builtin').grep_string({grep_open_files = true})<cr>", "Grep current string in open files" },
  --   },

  --   -- " Available Debug Adapters:
  --   -- "   https://microsoft.github.io/debug-adapter-protocol/implementors/adapters/
  --   -- " Adapter configuration and installation instructions:
  --   -- "   https://github.com/mfussenegger/nvim-dap/wiki/Debug-Adapter-installation
  --   -- " Debug Adapter protocol:
  --   -- "   https://microsoft.github.io/debug-adapter-protocol/
  --   d = {
  --     name = "Debug",
  --     t = { "<cmd>lua require'dap'.toggle_breakpoint()<cr>", "Toggle Breakpoint" },
  --     b = { "<cmd>lua require'dap'.step_back()<cr>", "Step Back" },
  --     c = { "<cmd>lua require'dap'.continue()<cr>", "Continue" },
  --     C = { "<cmd>lua require'dap'.run_to_cursor()<cr>", "Run To Cursor" },
  --     d = { "<cmd>lua require'dap'.disconnect()<cr>", "Disconnect" },
  --     g = { "<cmd>lua require'dap'.session()<cr>", "Get Session" },
  --     i = { "<cmd>lua require'dap'.step_into()<cr>", "Step Into" },
  --     o = { "<cmd>lua require'dap'.step_over()<cr>", "Step Over" },
  --     u = { "<cmd>lua require'dap'.step_out()<cr>", "Step Out" },
  --     p = { "<cmd>lua require'dap'.pause()<cr>", "Pause" },
  --     r = { "<cmd>lua require'dap'.repl.toggle()<cr>", "Toggle Repl" },
  --     s = { "<cmd>lua require'dap'.continue()<cr>", "Start" },
  --     q = { "<cmd>lua require'dap'.close()<cr>", "Quit" },
  --     U = { "<cmd>lua require'dapui'.toggle({reset = true})<cr>", "Toggle UI" },
  --   },
  --   p = {
  --     name = "Plugins",
  --     i = { "<cmd>Lazy install<cr>", "Install" },
  --     s = { "<cmd>Lazy sync<cr>", "Sync" },
  --     S = { "<cmd>Lazy clear<cr>", "Status" },
  --     c = { "<cmd>Lazy clean<cr>", "Clean" },
  --     u = { "<cmd>Lazy update<cr>", "Update" },
  --     p = { "<cmd>Lazy profile<cr>", "Profile" },
  --     l = { "<cmd>Lazy log<cr>", "Log" },
  --     d = { "<cmd>Lazy debug<cr>", "Debug" },
  --   },
  --   g = {
  --     name = "Git",
  --     g = { "<cmd>G<cr>", "Fugitive" },
  --     h = {
  --       name = "Hunk",
  --       j = { "<cmd>lua require 'gitsigns'.next_hunk({navigation_message = false})<cr>", "Next Hunk" },
  --       k = { "<cmd>lua require 'gitsigns'.prev_hunk({navigation_message = false})<cr>", "Prev Hunk" },
  --       p = { "<cmd>lua require 'gitsigns'.preview_hunk()<cr>", "Preview Hunk" },
  --       r = { "<cmd>lua require 'gitsigns'.reset_hunk()<cr>", "Reset Hunk" },
  --       R = { "<cmd>lua require 'gitsigns'.reset_buffer()<cr>", "Reset Buffer" },
  --       s = { "<cmd>lua require 'gitsigns'.stage_hunk()<cr>", "Stage Hunk" },
  --       u = { "<cmd>lua require 'gitsigns'.undo_stage_hunk()<cr>", "Undo Stage Hunk" },
  --     },
  --     l = { "<cmd>lua require 'gitsigns'.blame_line()<cr>", "Blame Line" },
  --     o = { "<cmd>lua require('telescope.builtin').git_status({cmd=vim.fn.expand('%:p:h')})<cr>", "Status" },
  --     b = { "<cmd>lua require('telescope.builtin').git_branches({cmd=vim.fn.expand('%:p:h')})<cr>", "Branches" },
  --     c = { "<cmd>lua require('telescope.builtin').git_commits({cmd=vim.fn.expand('%:p:h')})<cr>", "Commits" },
  --     C = { "<cmd>lua require('telescope.builtin').git_bcommits({cmd=vim.fn.expand('%:p:h')})<cr>", "Commits(for current file)" },
  --     d = { "<cmd>Gitsigns diffthis HEAD<cr>", "Git Diff" },
  --     z = { "<cmd>LazyGitCurrentFile<cr>", "Lazygit" },
  --   },
  --   l = {
  --     name = "LSP",
  --     d = {
  --       name = "Diagnostics",
  --       h = { "<cmd>lua vim.diagnostic.hide()<CR>lua vim.diagnostic.config({ signs = false, underline = false })<cr>", "Hide LSP Diagnostics" },
  --       s = { "<cmd>lua vim.diagnostic.show()<CR>lua vim.diagnostic.config({ signs = true, underline = true })<cr>", "Show LSP Diagnostics" },
  --     },
  --     a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Code Action" },
  --     b = { "<cmd>Telescope diagnostics bufnr=0 theme=get_ivy<cr>", "Buffer Diagnostics" },
  --     c = { vim.lsp.codelens.run, "CodeLens Action" },
  --     w = { "<cmd>Telescope diagnostics<cr>", "Diagnostics" },
  --     f = { vim.lsp.buf.format, "Format" },
  --     i = { "<cmd>LspInfo<cr>", "Info" },
  --     I = { "<cmd>Mason<cr>", "Mason Info" },
  --     j = { vim.diagnostic.goto_next, "Next Diagnostic" },
  --     k = { vim.diagnostic.goto_prev, "Prev Diagnostic" },
  --     l = { "<cmd>lua vim.diagnostic.open_float()<cr>", "Show Line Diagnostics" },
  --     q = { vim.diagnostic.setloclist, "Quickfix" },
  --     r = { vim.lsp.buf.rename, "Rename" },
  --     s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols" },
  --     S = { "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", "Workspace Symbols", },
  --     e = { "<cmd>Telescope quickfix<cr>", "Telescope Quickfix" },
  --   },
  --   s = {
  --     name = "Search",
  --     b = { "<cmd>lua require('telescope.builtin').live_grep({grep_open_files = true})<cr>", "Grep Open Files" },
  --     B = { "<cmd>lua require('telescope.builtin').grep_string({grep_open_files = true})<cr>", "Grep current string in open files" },
  --     f = { "<cmd>lua require('fzf-lua').files()<cr>", "Fzf.Lua Files" },
  --     F = { "<cmd>Telescope find_files<cr>", "Telescope Show File" },
  --     g = { "<cmd>Telescope grep_string<cr>", "Grep current string" },
  --     G = { "<cmd>Telescope grep_string<cr>", "Grep current string" },
  --     h = { "<cmd>Telescope help_tags<cr>", "Find Help" },
  --     H = { "<cmd>Telescope highlights<cr>", "Find highlight groups" },
  --     m = { "<cmd>Telescope marks<cr>", "Marks" },
  --     M = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
  --     r = { "<cmd>Telescope oldfiles<cr>", "Recent File" },
  --     R = { "<cmd>Telescope registers<cr>", "Registers" },
  --     s = { "<cmd>Telescope live_grep<cr>", "Live Grep" },
  --     S = { "<cmd>Telescope live_grep<cr>", "Live Grep" },
  --     T = { "<cmd>Telescope filetypes<cr>", "Set filetype" },
  --     k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
  --     C = { "<cmd>Telescope commands<cr>", "Commands" },
  --     c = { "<cmd>lua require('telescope.builtin').colorscheme({enable_preview = true})<cr>", "Colorscheme" },
  --   },
  --   T = {
  --     name = "Treesitter",
  --     i = { ":TSConfigInfo<cr>", "Info" },
  --   },
  -- },

  my_n_mappings_opts = {
    mode = "n", -- NORMAL mode
    prefix = "",
    buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
    silent = true, -- use `silent` when creating keymaps
    noremap = true, -- use `noremap` when creating keymaps
    nowait = true, -- use `nowait` when creating keymaps
  },
  my_n_mappings = {
    ['\\\\'] = { "<cmd>Telescope buffers<cr>", "List Buffers" },
  },
}

function M.setup()
  local status_ok, which_key = pcall(require, "which-key")
  if not status_ok then
    return
  end

  which_key.setup(M.config.config)

  which_key.register(M.config.normal_leader_mappings, M.config.normal_leader_opts)
  which_key.register(M.config.visual_leader_mappings, M.config.visual_leader_opts)
  which_key.register(M.config.my_n_mappings, M.config.my_n_mappings_opts)
end

return M
