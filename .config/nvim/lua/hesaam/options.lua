-- creates a backup file
vim.opt.backup = false

-- allows neovim to access the system clipboard
vim.opt.clipboard = "unnamedplus"

-- more space in the neovim command line for displaying messages
vim.opt.cmdheight = 2

-- mostly just for cmp
vim.opt.completeopt = { "menuone", "noselect" }

-- so that `` is visible in markdown files
vim.opt.conceallevel = 0

-- the encoding written to a file
vim.opt.fileencoding = "utf-8"

-- highlight all matches on previous search pattern
vim.opt.hlsearch = true

-- ignore case in search patterns
vim.opt.ignorecase = true

-- allow the mouse to be used in neovim
vim.opt.mouse = "a"

-- pop up menu height
vim.opt.pumheight = 10

-- we don't need to see things like -- INSERT -- anymore
-- vim.opt.showmode = false

-- always show tabs
-- vim.opt.showtabline = 1

-- Ignore case if search pattern is all lowercase, case-sensitive otherwise
vim.opt.smartcase = true

-- make indenting smarter again
-- vim.opt.smartindent = true

-- force all horizontal splits to go below current window
vim.opt.splitbelow = true

-- force all vertical splits to go to the right of current window
vim.opt.splitright = true

-- creates a swapfile
vim.opt.swapfile = false

-- time to wait for a mapped sequence to complete (in milliseconds)
vim.opt.timeoutlen = 300

-- enable persistent undo
-- vim.opt.undofile = true

-- faster completion (4000ms default)
vim.opt.updatetime = 300

-- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
vim.opt.writebackup = false

-- convert tabs to spaces
vim.opt.expandtab = true

-- Prefer Unix
vim.opt.fileformats = 'unix,dos,mac'

-- the number of spaces inserted for each indentation
vim.opt.shiftwidth = 2

-- insert 2 spaces for a tab
vim.opt.tabstop = 2

-- highlight the current line
vim.opt.cursorline = true

-- set numbered lines
vim.opt.number = true

-- set relative numbered lines
vim.opt.relativenumber = false

-- set number column width to 2 {default 4}
vim.opt.numberwidth = 4

-- always show the sign column, otherwise it would shift the text each time
vim.opt.signcolumn = "yes"

-- display lines as one long line
vim.opt.wrap = true

-- companion to wrap, don't split words
vim.opt.linebreak = true

-- No backups left after done editing
-- vim.opt.nobackup = true

-- minimal number of screen lines to keep above and below the cursor
vim.opt.scrolloff = 8

--  Change the terminal's title
vim.opt.title = true

-- minimal number of screen columns either side of cursor if wrap is `false`
vim.opt.sidescrolloff = 8

-- the font used in graphical neovim applications
vim.opt.guifont = "monospace:h17"

-- which "horizontal" keys are allowed to travel to prev/next line
-- vim.opt.whichwrap = "bs<>[]hl"

-- flags to shorten vim messages, see :help 'shortmess'
-- vim.opt.shortmess = "ilmnrx"

-- don't give |ins-completion-menu| messages
vim.opt.shortmess:append "c"

-- hyphenated words recognized by searches
vim.opt.iskeyword:append "-"

-- don't insert the current comment leader automatically for
-- auto-wrapping comments using 'textwidth', hitting <Enter> in insert mode,
-- or hitting 'o' or 'O' in normal mode.
vim.opt.formatoptions:remove({ "c", "r", "o" })

-- separate vim plugins from neovim in case vim still in use
vim.opt.runtimepath:remove("/usr/share/vim/vimfiles")

-- vim.opt.foldmethod = 'expr'
-- vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
-- vim.cmd [[set nofoldenable]]
--
