vim.g.mapleader = " "

-- Emacs-like bindings in insert mode
vim.keymap.set('i', '<C-e>', '<C-o>$')
vim.keymap.set('i', '<C-a>', '<C-o>^')
vim.keymap.set('i', '<C-q>', '<C-o>0')


-- Emacs-like bnormal mode from `:h emacs-keys`
vim.keymap.set('n', '<C-a>', '^')
vim.keymap.set('n', '<C-q>', '<Home>')
vim.keymap.set('n', '<C-d>', '<Del>')
vim.keymap.set('n', '<C-e>', '<End>')

-- Emacs-like bindings in the command line from `:h emacs-keys`
vim.keymap.set('c', '<C-a>', '<Home>')
vim.keymap.set('c', '<C-b>', '<Left>')
vim.keymap.set('c', '<C-f>', '<Right>')
vim.keymap.set('c', '<C-d>', '<Del>')
vim.keymap.set('c', '<C-e>', '<End>')
vim.keymap.set('c', '<M-b>', '<S-Left>')
vim.keymap.set('c', '<M-f>', '<S-Right>')
vim.keymap.set('c', '<M-d>', '<S-right><Delete>')
vim.keymap.set('c', '<Esc>b', '<S-Left>')
vim.keymap.set('c', '<Esc>f', '<S-Right>')
vim.keymap.set('c', '<Esc>d', '<S-right><Delete>')
vim.keymap.set('c', '<C-g>', '<C-c>')

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")
