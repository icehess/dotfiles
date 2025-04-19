local M = {}

M.config = {
  ui = {
    border = "rounded",
  }
}

function M.setup()
  xpcall(function()
    local mason  = require("mason")

    mason.setup(M.config)
  end, function()
      print("Failed to load mason")
  end
  )
end

return M
