local M = {}

M.config = {
  ui = {
    border = "rounded",
  }
}

function M.setup()
  local status_ok, mason = pcall(require, "mason")
  if not status_ok then
    return
  end

  mason.setup(M.config)
end

return M
