local M = {}

local colors = {
  blue   = '#80a0ff',
  cyan   = '#79dac8',
  black  = '#080808',
  white  = '#c6c6c6',
  red    = '#ff5189',
  violet = '#d183e8',
  grey   = '#303030',
}

local function search_result()
  if vim.v.hlsearch == 0 then
    return ''
  end
  local last_search = vim.fn.getreg('/')
  if not last_search or last_search == '' then
    return ''
  end
  local searchcount = vim.fn.searchcount { maxcount = 9999 }
  return last_search .. '(' .. searchcount.current .. '/' .. searchcount.total .. ')'
end

local bubbles_theme = {
  normal = {
    a = { fg = colors.black, bg = colors.cyan },
    b = { fg = colors.white, bg = colors.grey },
    c = { fg = colors.black, bg = colors.black },
  },

  insert = { a = { fg = colors.black, bg = colors.blue } },
  visual = { a = { fg = colors.black, bg = colors.violet } },
  replace = { a = { fg = colors.black, bg = colors.red } },

  inactive = {
    a = { fg = colors.white, bg = colors.black },
    b = { fg = colors.white, bg = colors.black },
    c = { fg = colors.black, bg = colors.black },
  },
}

M.config = {
  options = {
    theme = bubbles_theme,
    -- theme = 'ayu_dark',
    component_separators = '|',
    section_separators = { left = '', right = '' },
    -- section_separators = { left = '', right = '' },
    globalstatus = true,
  },
  sections = {
    lualine_a = {
      { 'mode', right_padding = 2 } -- separator = { left = '' }, right_padding = 2 },
    },
    lualine_b = { 'filename', 'branch', 'diff' },
    lualine_c = { 'diagnostics' },
    lualine_x = {},
    lualine_y = { search_result, 'filetype', 'progress' },
    lualine_z = {
      { 'location', left_padding = 2 } -- separator = { right = '' }, left_padding = 2 },
    },
  },
  -- inactive_sections = {
  --   lualine_a = { 'filename' },
  --   lualine_b = {},
  --   lualine_c = {},
  --   lualine_x = {},
  --   lualine_y = {},
  --   lualine_z = { 'location' },
  -- },
  tabline = {},
  extensions = {},
}

function M.setup()
  xpcall(function()
    local lualine = require("lualine")

    lualine.setup(M.config)
  end, function()
      print("Failed to load lualine")
  end
  )
end

return M
