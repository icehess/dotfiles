require('rose-pine').setup({
  disable_background = true,
  dark_variant = 'main',
})

-- set term gui colors (most terminals support this)
vim.opt.termguicolors = true

function ColorMyPencils(color, background, nobg)
  color = color or "rasmus"
  background = background or "dark"
  nobg = nobg or true
  vim.cmd.colorscheme(color)
  vim.o.background = background

  if nobg then
    vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
  end

end

ColorMyPencils()
