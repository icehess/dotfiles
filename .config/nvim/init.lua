local path_sep = vim.loop.os_uname().version:match "Windows" and "\\" or "/"

---Join path segments that were passed as input
---@return string
function _G.join_paths(...)
  local result = table.concat({ ... }, path_sep)
  return result
end

-- if vim.fn.has "nvim-0.8" ~= 1 then
if os.getenv "GIVE_ME_SHINY_NVIM" ~= 'true' then
  vim.opt.rtp:remove(join_paths(vim.call("stdpath", "config")))
  vim.opt.rtp:remove(join_paths(vim.call("stdpath", "config"), "plugin"))
  vim.opt.rtp:remove(join_paths(vim.call("stdpath", "config"), "after"))
  vim.opt.rtp:remove(join_paths(vim.call("stdpath", "data"), "site"))
  vim.opt.rtp:remove(join_paths(vim.call("stdpath", "data"), "site", "after"))
  vim.opt.rtp:prepend(join_paths('~', '.vim'))
  vim.opt.rtp:append(join_paths('~', '.vim', 'after'))
  vim.cmd("let &packpath = &runtimepath")
  vim.cmd('source ' .. join_paths('~', '.vim', 'vimrc'))
  return
end

require("hesaam")
