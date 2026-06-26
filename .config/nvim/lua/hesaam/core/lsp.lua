local M = {}

M.config = {}

function M.setup()
  xpcall(function()

    local lspconfig = require "mason-lspconfig"
    lspconfig.setup {
      ensure_installed = {
        'clangd',
        'elp',
        'eslint',
        'expert',
        'lua_ls',
        'ruff',
        'rust_analyzer',
        'ts_ls',
        'vimls',
      },
    }

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

    -- vim.diagnostic.config({
    --   virtual_text = false,
    --   float = {
    --     border = "single",
    --   },
    -- })

    -- local none_ls = require "none-ls"

    -- none_ls.setup({
    --   sources = {
    --     -- Replace these with the tools you have installed
    --     none_ls.builtins.formatting.prettier,
    --     none_ls.builtins.diagnostics.eslint,
    --     none_ls.builtins.formatting.stylua,
    --   }
    -- })
  end, function()
      print("Failed to load lsp")
  end
  )
end

return M
