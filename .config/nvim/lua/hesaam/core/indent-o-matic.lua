local M = {}

M.config = {
  -- The values indicated here are the defaults

  -- Number of lines without indentation before giving up (use -1 for infinite)
  max_lines = 2048,
  -- Space indentations that should be detected
  standard_widths = { 2, 4, 8 },
  -- Skip multi-line comments and strings (more accurate detection but less performant)
  skip_multiline = true,
}

function M.setup()
  local status_ok, indent_o_matic = pcall(require, "ident-o-matic")
  if not status_ok then
    print "Failed to load ident-o-matic"
    return
  end

  indent_o_matic.setup(M.config)
end

return M
