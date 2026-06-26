local M = {}


M.config = {}

function M.setup()
  xpcall(function()
    local gitsigns = require("gitsigns")

    gitsigns.setup({})
  end, function()
    print "Failed to load gitsigns"
  end
  )
end

return M
