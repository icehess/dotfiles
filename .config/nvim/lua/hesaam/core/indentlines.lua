local M = {}

local icons = require "hesaam.icons"

M.config = {
  enabled = true,
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
  local status_ok, indentlines = pcall(require, "indentlines")
  if not status_ok then
    return
  end

  indentlines.setup(M.config)
end

return M
