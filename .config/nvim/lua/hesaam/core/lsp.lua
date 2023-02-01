local M = {}

M.config = {
  ensure_installed = {
    'erlangls',
    'eslint',
    'rust_analyzer',
    'sumneko_lua',
    'tsserver',
    'vimls',
  },
  opts = {
    border = "rounded",
  },
  sumneko_lua = {
    settings = {
      Lua = {
        diagnostics = {
          globals = { 'vim' }
        }
      }
    }
  },
  set_preferences = {
    suggest_lsp_servers = true,
    sign_icons = {
      error = 'E',
      warn = 'W',
      hint = 'H',
      info = 'I'
    }
  },
}

function M.setup()
  local status_ok, lsp_zero = pcall(require, "lsp-zero")
  if not status_ok then
    return
  end

  lsp_zero.preset("recommended")
  lsp_zero.ensure_installed(M.config.ensure_installed)

  -- Fix Undefined global 'vim'
  lsp_zero.configure('sumneko_lua', M.config.sumneko_lua)

  -- local cmp_select = {behavior = cmp.SelectBehavior.Select}
  -- local cmp_mappings = lsp.defaults.cmp_mappings({
  --         ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
  --         ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
  --         ['<C-y>'] = cmp.mapping.confirm({ select = true }),
  --         ["<C-Space>"] = cmp.mapping.complete(),
  --     })

  -- disable completion with tab
  -- this helps with copilot setup
  -- cmp_mappings['<Tab>'] = nil
  -- cmp_mappings['<S-Tab>'] = nil

  -- lsp.setup_nvim_cmp({
  --         mapping = cmp_mappings
  --     })

  lsp_zero.set_preferences(M.config.set_preferences)

  -- lsp.on_attach(function(client, bufnr)
  --     local opts = {buffer = bufnr, remap = false}

  --     if client.name == "eslint" then
  --         vim.cmd.LspStop('eslint')
  --         return
  --     end

  --     vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
  --     vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
  --     vim.keymap.set("n", "<leader>vws", vim.lsp.buf.workspace_symbol, opts)
  --     vim.keymap.set("n", "<leader>vd", vim.diagnostic.open_float, opts)
  --     vim.keymap.set("n", "[d", vim.diagnostic.goto_next, opts)
  --     vim.keymap.set("n", "]d", vim.diagnostic.goto_prev, opts)
  --     vim.keymap.set("n", "<leader>vca", vim.lsp.buf.code_action, opts)
  --     vim.keymap.set("n", "<leader>vrr", vim.lsp.buf.references, opts)
  --     vim.keymap.set("n", "<leader>vrn", vim.lsp.buf.rename, opts)
  --     vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, opts)
  -- end)

  lsp_zero.setup()

  vim.diagnostic.config({
    virtual_text = false,
    float = {
      border = "single",
    },
  })
end

return M
