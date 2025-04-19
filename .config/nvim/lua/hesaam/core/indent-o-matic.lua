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
  xpcall(function()
    local indent_o_matic = require("ident-o-matic")

    indent_o_matic.setup(M.config)
  end, function()
    print "Failed to load ident-o-matic"
  end
  )
end

return M
