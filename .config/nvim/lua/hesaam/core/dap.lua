local M = {}

local icons = require "hesaam.icons"

M.config = {
  active = true,
  on_config_done = nil,
  breakpoint = {
    text = icons.ui.Bug,
    texthl = "DiagnosticSignError",
    linehl = "",
    numhl = "",
  },
  breakpoint_rejected = {
    text = icons.ui.Bug,
    texthl = "DiagnosticSignError",
    linehl = "",
    numhl = "",
  },
  stopped = {
    text = icons.ui.BoldArrowRight,
    texthl = "DiagnosticSignWarn",
    linehl = "Visual",
    numhl = "DiagnosticSignWarn",
  },
  log = {
    level = "info",
  },
  ui = {
    auto_open = true,
    notify = {
      threshold = vim.log.levels.INFO,
    },
    config = {
      icons = { expanded = "", collapsed = "", circular = "" },
      mappings = {
        -- Use a table to apply multiple mappings
        expand = { "<CR>", "<2-LeftMouse>" },
        open = "o",
        remove = "d",
        edit = "e",
        repl = "r",
        toggle = "t",
      },
      -- Use this to override mappings for specific elements
      element_mappings = {},
      expand_lines = true,
      layouts = {
        {
          elements = {
            { id = "scopes", size = 0.33 },
            { id = "breakpoints", size = 0.17 },
            { id = "stacks", size = 0.25 },
            { id = "watches", size = 0.25 },
          },
          size = 0.33,
          position = "right",
        },
        {
          elements = {
            { id = "repl", size = 0.45 },
            { id = "console", size = 0.55 },
          },
          size = 0.27,
          position = "bottom",
        },
      },
      controls = {
        enabled = true,
        -- Display controls in this element
        element = "repl",
        icons = {
          pause = "",
          play = "",
          step_into = "",
          step_over = "",
          step_out = "",
          step_back = "",
          run_last = "",
          terminate = "",
        },
      },
      floating = {
        max_height = 0.9,
        max_width = 0.5, -- Floats will be treated as percentage of your screen.
        border = vim.g.border_chars, -- Border style. Can be 'single', 'double' or 'rounded'
        mappings = {
          close = { "q", "<Esc>" },
        },
      },
      windows = { indent = 1 },
      render = {
        max_type_length = nil, -- Can be integer or nil.
        max_value_lines = 100, -- Can be integer or nil.
      },
    },
  },
}

function M.setup()
  xpcall(function()
    local dap = require("dap")

    vim.fn.sign_define("DapBreakpoint", M.config.breakpoint)
    vim.fn.sign_define("DapBreakpointRejected", M.config.breakpoint_rejected)
    vim.fn.sign_define("DapStopped", M.config.stopped)

    dap.set_log_level(M.config.log.level)
  end, function()
      print("Failed to load dap")
  end
  )
end

M.setup_ui = function()
  xpcall(function()
    local dap = require("dap")

    local dapui = require "dapui"
    dapui.setup(M.config.ui.config)

    if M.config.ui.auto_open then
      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open()
      end
      -- dap.listeners.before.event_terminated["dapui_config"] = function()
      --   dapui.close()
      -- end
      -- dap.listeners.before.event_exited["dapui_config"] = function()
      --   dapui.close()
      -- end
    end

    -- local Log = require "lvim.core.log"

    -- -- until rcarriga/nvim-dap-ui#164 is fixed
    -- local function notify_handler(msg, level, opts)
    --   if level >= M.config.ui.notify.threshold then
    --     return vim.notify(msg, level, opts)
    --   end

    --   opts = vim.tbl_extend("keep", opts or {}, {
    --     title = "dap-ui",
    --     icon = "",
    --     on_open = function(win)
    --       vim.api.nvim_buf_set_option(vim.api.nvim_win_get_buf(win), "filetype", "markdown")
    --     end,
    --   })

    --   -- vim_log_level can be omitted
    --   if level == nil then
    --     level = 3 -- Log.levels["INFO"]
    --   elseif type(level) == "string" then
    --     level = 3 -- Log.levels[(level):upper()] or Log.levels["INFO"]
    --   else
    --     -- https://github.com/neovim/neovim/blob/685cf398130c61c158401b992a1893c2405cd7d2/runtime/lua/vim/lsp/log.lua#L5
    --     level = level + 1
    --   end

    --   msg = string.format("%s: %s", opts.title, msg)
    --   Log:add_entry(level, msg)
    -- end

    -- local _, _ = xpcall(function()
    -- -- local dapui_ok, _ = xpcall(function()
    --   -- require("dapui.util").notify = notify_handler
    -- end, debug.traceback)
    -- if not dapui_ok then
    --   Log:debug "Unable to override dap-ui logging level"
    -- end
  end, function()
      print("Failed to load dap ui")
  end
  )
end

return M
