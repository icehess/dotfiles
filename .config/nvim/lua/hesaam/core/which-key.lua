local M = {}

M.config = {
  config = {
    ---@type false | "classic" | "modern" | "helix"
    preset = "classic",
    delay = function(ctx)
      return ctx.plugin and 0 or 200
    end,

    spec = {
      { "\\\\", "<cmd>Telescope buffers<cr>", desc = "List Buffers", nowait = true, remap = false },
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
      { "<leader>gbb", "<cmd>lua require('telescope.builtin').git_branches({cmd=vim.fn.expand('%:p:h')})<cr>", desc = "Branches", nowait = true, remap = false },
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

      {
        mode = { "v" },
        {
          "<leader>/",
          "<Plug>(comment_toggle_linewise_visual)",
          desc = "Comment toggle linewise (visual)",
          nowait = true,
          remap = false,
        },
        { "<leader>g", group = "Git", nowait = true, remap = false },
        { "<leader>gr", "<cmd>Gitsigns reset_hunk<cr>", desc = "Reset Hunk", nowait = true, remap = false },
        { "<leader>gs", "<cmd>Gitsigns stage_hunk<cr>", desc = "Stage Hunk", nowait = true, remap = false },
        { "<leader>l", group = "LSP", nowait = true, remap = false },
        {
          "<leader>la",
          "<cmd>lua vim.lsp.buf.code_action()<cr>",
          desc = "Code Action",
          nowait = true,
          remap = false,
        },
      },
    },
    -- show a warning when issues were detected with your mappings
    notify = true,
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
    --- -- @type wk.Win
    win = {
      -- don't allow the popup to overlap with the cursor
      no_overlap = true,
      -- width = 1,
      -- height = { min = 4, max = 25 },
      -- col = 0,
      -- row = math.huge,
      -- border = "none",
      border = "single",
      padding = { 2, 2, 2, 2 }, -- extra window padding [top/bottom, right/left]
      title = true,
      title_pos = "center",
      zindex = 1000,
      -- Additional vim.wo and vim.bo options
      bo = {},
      wo = {
        winblend = 0,
      },
    },
    layout = {
      height = { min = 4, max = 25 }, -- min and max height of the columns
      width = { min = 20, max = 50 }, -- min and max width of the columns
      spacing = 3, -- spacing between columns
      align = "left", -- align columns left, center or right
    },
    keys = {
      scroll_down = "<c-d>", -- binding to scroll down inside the popup
      scroll_up = "<c-u>", -- binding to scroll up inside the popup
    },
    --- Add "manual" as the first element to use the order the mappings were registered
    --- Other sorters: "desc"
    sort = { "local", "order", "group", "alphanum", "mod", "lower", "icase" },
    expand = 1, -- expand groups when <= n mappings
    replace = {
      key = {
        function(key)
          return require("which-key.view").format(key)
        end,
        -- { "<Space>", "SPC" },
      },
      desc = {
        { "<Plug>%((.*)%)", "%1" },
        { "^%+", "" },
        { "<[cC]md>", "" },
        { "<[cC][rR]>", "" },
        { "<[sS]ilent>", "" },
        { "^lua%s+", "" },
        { "^call%s+", "" },
        { "^:%s*", "" },
      },
    },
    icons = {
      breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
      separator = "➜", -- symbol used between a key and it's label
      group = "+", -- symbol prepended to a group
      mappings = false,
    },
    show_help = true, -- show help message on the command line when the popup is visible
    show_keys = true, -- show the currently pressed key and its label as a message in the command line
    triggers = { "<auto>", mode = "nixsotc" }, -- automatically setup triggers
    disable = {
      -- disable WhichKey for certain buf types and file types.
      ft = {},
      bt = { "TelescopePrompt" },
    },
    debug = false, -- enable wk.log in the current directory

    mappings = {},
  },
}

function M.setup()
  xpcall(function()
    local which_key = require("which-key")

    which_key.setup(M.config.config)
  end, function()
    print "Failed to load which_key "
  end
  )
end

return M
