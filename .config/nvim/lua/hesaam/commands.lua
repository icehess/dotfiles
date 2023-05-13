vim.api.nvim_create_user_command('CLEAN', function()
  vim.cmd("retab | %s/\\s\\+$//")
end, {
  force = true,
  desc = "Clean trailing whitespaces and retab the current buffer",
})

vim.api.nvim_create_augroup("restorCursor", {})
vim.api.nvim_create_autocmd({ "BufWinEnter" }, {
  group = "restorCursor",
  pattern = "*",
  callback = function()
    if vim.fn.line("'\"") <= vim.fn.line("$") then
      vim.cmd [[normal! g`"]]
    end
  end
})

-- vim.api.nvim_create_user_command('Rg', function(opts)
--   local search_dirs = opts.fargs
--   require('telescope.builtin').live_grep({ search_dirs = search_dirs })
-- end, {
--   nargs = "*",
--   force = true,
--   desc = "Search using riprep via Telescope in these paths"
-- })
