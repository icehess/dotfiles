local M = {}

local icons = require "hesaam.icons"

M.config = {
  enabled = false,
  buftype_exclude = { "terminal", "nofile" },
  filetype_exclude = {
    "help",
    "startify",
    "dashboard",
    "lazy",
    "neogitstatus",
    "NvimTree",
    "Trouble",
    "text",
  },
  char = icons.ui.LineLeft,
  context_char = icons.ui.LineLeft,
  show_trailing_blankline_indent = false,
  show_first_indent_level = true,
  use_treesitter = true,
  show_current_context = true,
}

function M.setup()
  xpcall(function()
    local indentlines  = require("indentlines")

    vim.g.indent_blankline_enabled = false
    vim.cmd[[set g:indent_blankline_enabled = false]]
    indentlines.setup(M.config)
  end, function()
      print("Failed to load indentlines")
  end
  )
end

return M
