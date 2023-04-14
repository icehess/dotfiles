require('rose-pine').setup({
  disable_background = true,
  dark_variant = 'main',
})

-- set term gui colors (most terminals support this)
vim.opt.termguicolors = true

vim.g.gruvbox_material_background = 'hard'
vim.g.gruvbox_material_better_performance = 1
vim.g.gruvbox_material_foreground = 'original' -- material or mix, original
vim.g.gruvbox_material_diagnostic_text_highlight = 1
vim.g.gruvbox_material_current_word = 'bold' -- or bold, underline, italic
vim.g.gruvbox_material_enable_bold = 1
vim.g.gruvbox_material_diagnostic_text_highlight = 1
vim.g.gruvbox_material_diagnostic_line_highlight = 1
vim.g.gruvbox_material_transparent_background = 2

function ColorMyPencils(color, background, nobg)
  color = color or "gruvbox-material" -- "blue" -- or "habamax"
  background = background or "dark"
  nobg = nobg or true
  vim.o.background = background
  vim.cmd.colorscheme(color)

  if nobg then
    vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
    vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = "none" })
  end

end

ColorMyPencils()

-- Make sure we highlight extra whitespace in the most annoying way possible.
vim.cmd [[highlight SpecialKey ctermbg=Yellow guibg=Yellow]]

-- Make trailing spaces very visible
vim.cmd [[highlight ExtraWhitespace ctermbg=Yellow guibg=Yellow]]
vim.cmd [[match ExtraWhitespace /\s\+$/]]
