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
    hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
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
    [";"] = { "<cmd>Alpha<CR>", "Dashboard" },
    ["w"] = { "<cmd>w!<CR>", "Save" },
    ["q"] = { "<cmd>confirm q<CR>", "Quit" },
    ["/"] = { "<Plug>(comment_toggle_linewise_current)", "Comment toggle current line" },
    ["c"] = { "<cmd>lua require('hesaam.core.bufferline').buf_kill 'bd'<CR>", "Close Buffer" },
    ["f"] = { "<cmd>Telescope find_files<CR>", "Telescope Find File" },
    ["F"] = { "<cmd>Files<cr>", "Fzf Files" },
    ["h"] = { "<cmd>nohlsearch<CR>", "No Highlight" },
    ["e"] = { "<cmd>NvimTreeToggle<CR>", "Explorer" },
    ["\\"] = {
      name = "Useful Keymaps",
      A = { "<cmd>set noexpandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>", "Use Tab instead of space" },
      s = { "<cmd>setlocal invspell<CR>", "Highlight spell Check" },
      g = { "<cmd>Gitsigns toggle_signs<cr>", "Toggle Gitsigns" },
      n = { "<cmd>setlocal number!<cr>:setlocal number?<cr>", "Toggle line number" },
    },
    b = {
      name = "Buffers",
      j = { "<cmd>BufferLinePick<cr>", "Jump" },
      -- j = { "<cmd>Telescope buffers<cr>", "Jump" },
      f = { "<cmd>Telescope buffers previewer=false<cr>", "Find" },
      b = { "<cmd>BufferLineCyclePrev<cr>", "Previous" },
      n = { "<cmd>BufferLineCycleNext<cr>", "Next" },
      W = { "<cmd>noautocmd w<cr>", "Save without formatting (noautocmd)" },
      -- w = { "<cmd>BufferWipeout<cr>", "Wipeout" }, -- TODO: implement this for bufferline
      e = { "<cmd>BufferLinePickClose<cr>", "Pick which buffer to close" },
      h = { "<cmd>BufferLineCloseLeft<cr>", "Close all to the left" },
      l = { "<cmd>BufferLineCloseRight<cr>", "Close all to the right" },
      D = { "<cmd>BufferLineSortByDirectory<cr>", "Sort by directory" },
      L = { "<cmd>BufferLineSortByExtension<cr>", "Sort by language" },
      s = { "<cmd>lua require('telescope.builtin').live_grep({grep_open_files = true})<cr>", "Grep Open Files" },
      S = { "<cmd>lua require('telescope.builtin').grep_string({grep_open_files = true})<cr>", "Grep current string in open files" },
    },

    -- " Available Debug Adapters:
    -- "   https://microsoft.github.io/debug-adapter-protocol/implementors/adapters/
    -- " Adapter configuration and installation instructions:
    -- "   https://github.com/mfussenegger/nvim-dap/wiki/Debug-Adapter-installation
    -- " Debug Adapter protocol:
    -- "   https://microsoft.github.io/debug-adapter-protocol/
    d = {
      name = "Debug",
      t = { "<cmd>lua require'dap'.toggle_breakpoint()<cr>", "Toggle Breakpoint" },
      b = { "<cmd>lua require'dap'.step_back()<cr>", "Step Back" },
      c = { "<cmd>lua require'dap'.continue()<cr>", "Continue" },
      C = { "<cmd>lua require'dap'.run_to_cursor()<cr>", "Run To Cursor" },
      d = { "<cmd>lua require'dap'.disconnect()<cr>", "Disconnect" },
      g = { "<cmd>lua require'dap'.session()<cr>", "Get Session" },
      i = { "<cmd>lua require'dap'.step_into()<cr>", "Step Into" },
      o = { "<cmd>lua require'dap'.step_over()<cr>", "Step Over" },
      u = { "<cmd>lua require'dap'.step_out()<cr>", "Step Out" },
      p = { "<cmd>lua require'dap'.pause()<cr>", "Pause" },
      r = { "<cmd>lua require'dap'.repl.toggle()<cr>", "Toggle Repl" },
      s = { "<cmd>lua require'dap'.continue()<cr>", "Start" },
      q = { "<cmd>lua require'dap'.close()<cr>", "Quit" },
      U = { "<cmd>lua require'dapui'.toggle({reset = true})<cr>", "Toggle UI" },
    },
    p = {
      name = "Plugins",
      i = { "<cmd>Lazy install<cr>", "Install" },
      s = { "<cmd>Lazy sync<cr>", "Sync" },
      S = { "<cmd>Lazy clear<cr>", "Status" },
      c = { "<cmd>Lazy clean<cr>", "Clean" },
      u = { "<cmd>Lazy update<cr>", "Update" },
      p = { "<cmd>Lazy profile<cr>", "Profile" },
      l = { "<cmd>Lazy log<cr>", "Log" },
      d = { "<cmd>Lazy debug<cr>", "Debug" },
    },
    g = {
      name = "Git",
      g = { "<cmd>G<cr>", "Fugitive" },
      h = {
        name = "Hunk",
        j = { "<cmd>lua require 'gitsigns'.next_hunk({navigation_message = false})<cr>", "Next Hunk" },
        k = { "<cmd>lua require 'gitsigns'.prev_hunk({navigation_message = false})<cr>", "Prev Hunk" },
        p = { "<cmd>lua require 'gitsigns'.preview_hunk()<cr>", "Preview Hunk" },
        r = { "<cmd>lua require 'gitsigns'.reset_hunk()<cr>", "Reset Hunk" },
        R = { "<cmd>lua require 'gitsigns'.reset_buffer()<cr>", "Reset Buffer" },
        s = { "<cmd>lua require 'gitsigns'.stage_hunk()<cr>", "Stage Hunk" },
        u = { "<cmd>lua require 'gitsigns'.undo_stage_hunk()<cr>", "Undo Stage Hunk" },
      },
      l = { "<cmd>lua require 'gitsigns'.blame_line()<cr>", "Blame Line" },
      o = { "<cmd>lua require('telescope.builtin').git_status({cmd=vim.fn.expand('%:p:h')})<cr>", "Status" },
      b = { "<cmd>lua require('telescope.builtin').git_branches({cmd=vim.fn.expand('%:p:h')})<cr>", "Branches" },
      c = { "<cmd>lua require('telescope.builtin').git_commits({cmd=vim.fn.expand('%:p:h')})<cr>", "Commits" },
      C = { "<cmd>lua require('telescope.builtin').git_bcommits({cmd=vim.fn.expand('%:p:h')})<cr>", "Commits(for current file)" },
      d = { "<cmd>Gitsigns diffthis HEAD<cr>", "Git Diff" },
      z = { "<cmd>LazyGitCurrentFile<cr>", "Lazygit" },
    },
    l = {
      name = "LSP",
      d = {
        name = "Diagnostics",
        h = { "<cmd>lua vim.diagnostic.hide()<CR>lua vim.diagnostic.config({ signs = false, underline = false })<cr>", "Hide LSP Diagnostics" },
        s = { "<cmd>lua vim.diagnostic.show()<CR>lua vim.diagnostic.config({ signs = true, underline = true })<cr>", "Show LSP Diagnostics" },
      },
      a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Code Action" },
      b = { "<cmd>Telescope diagnostics bufnr=0 theme=get_ivy<cr>", "Buffer Diagnostics" },
      c = { vim.lsp.codelens.run, "CodeLens Action" },
      w = { "<cmd>Telescope diagnostics<cr>", "Diagnostics" },
      f = { vim.lsp.buf.format, "Format" },
      i = { "<cmd>LspInfo<cr>", "Info" },
      I = { "<cmd>Mason<cr>", "Mason Info" },
      j = { vim.diagnostic.goto_next, "Next Diagnostic" },
      k = { vim.diagnostic.goto_prev, "Prev Diagnostic" },
      l = { "<cmd>lua vim.diagnostic.open_float()<cr>", "Show Line Diagnostics" },
      q = { vim.diagnostic.setloclist, "Quickfix" },
      r = { vim.lsp.buf.rename, "Rename" },
      s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols" },
      S = { "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", "Workspace Symbols", },
      e = { "<cmd>Telescope quickfix<cr>", "Telescope Quickfix" },
    },
    s = {
      name = "Search",
      b = { "<cmd>lua require('telescope.builtin').live_grep({grep_open_files = true})<cr>", "Grep Open Files" },
      B = { "<cmd>lua require('telescope.builtin').grep_string({grep_open_files = true})<cr>", "Grep current string in open files" },
      f = { "<cmd>lua require('fzf-lua').files()<cr>", "Fzf.Lua Files" },
      F = { "<cmd>Telescope find_files<cr>", "Telescope Show File" },
      g = { "<cmd>Telescope grep_string<cr>", "Grep current string" },
      G = { "<cmd>Telescope grep_string<cr>", "Grep current string" },
      h = { "<cmd>Telescope help_tags<cr>", "Find Help" },
      H = { "<cmd>Telescope highlights<cr>", "Find highlight groups" },
      m = { "<cmd>Telescope marks<cr>", "Marks" },
      M = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
      r = { "<cmd>Telescope oldfiles<cr>", "Recent File" },
      R = { "<cmd>Telescope registers<cr>", "Registers" },
      s = { "<cmd>Telescope live_grep<cr>", "Live Grep" },
      S = { "<cmd>Telescope live_grep<cr>", "Live Grep" },
      T = { "<cmd>Telescope filetypes<cr>", "Set filetype" },
      k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
      C = { "<cmd>Telescope commands<cr>", "Commands" },
      c = { "<cmd>lua require('telescope.builtin').colorscheme({enable_preview = true})<cr>", "Colorscheme" },
    },
    T = {
      name = "Treesitter",
      i = { ":TSConfigInfo<cr>", "Info" },
    },
  },

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
