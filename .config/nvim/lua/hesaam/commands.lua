vim.api.nvim_create_user_command('CLEAN', function()
  vim.cmd("retab | %s/\\s\\+$//")
end, { force = true })

vim.api.nvim_create_augroup("restorCursor", {})
vim.api.nvim_create_autocmd({ "BufWinEnter" }, {
  group = "restorCursor",
  pattern = "*",
  callback = function ()
    if vim.fn.line("'\"") <= vim.fn.line("$") then
      vim.cmd[[normal! g`"]]
    end
  end
})
