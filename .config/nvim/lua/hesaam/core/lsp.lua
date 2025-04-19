local M = {}

M.config = {
  ensure_installed = {
    'erlangls',
    'eslint',
    'rust_analyzer',
    'lua_ls',
    'ts_ls',
    'vimls',
  },
  lua_ls = {
    settings = {
      Lua = {
        diagnostics = {
          -- Get the language server to recognize the `vim` global
          globals = { 'vim' }
        }
      }
    }
  },
  erlangls = {
    cmd = {
      "erlang_ls",
      -- "--log-level",
      -- "debug",
    }
  },
  opts = {
    border = "rounded",
  },
  set_preferences = {
    suggest_lsp_servers = false,
    sign_icons = {
      error = 'E',
      warn = 'W',
      hint = 'H',
      info = 'I'
    }
  },
}

function M.setup()
  xpcall(function()
    local lsp_zero = require("lsp-zero")

    local lspconfig = require "lspconfig"
    lspconfig.util.default_config = vim.tbl_extend(
          "force",
          lspconfig.util.default_config,
          {
            autostart = true,
          }
      )

    lsp_zero.preset({
      float_border = 'rounded',
      call_servers = 'local',
      configure_diagnostics = true,
      setup_servers_on_start = true,
      set_lsp_keymaps = {
        preserve_mappings = false,
        omit = {},
      },
      manage_nvim_cmp = {
        set_sources = 'recommended',
        set_basic_mappings = true,
        set_extra_mappings = false,
        use_luasnip = true,
        set_format = true,
        documentation_window = true,
      },
    })

    lsp_zero.ensure_installed(M.config.ensure_installed)

    lsp_zero.configure('lua_ls', M.config.lua_ls)
    lsp_zero.configure('erlangls', M.config.erlangls)

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

    local null_ls = require "null-ls"

    local null_opts = lsp_zero.build_options('null-ls', {})

    null_ls.setup({
      on_attach = function(client, bufnr)
        null_opts.on_attach(client, bufnr)
        ---
        -- you can add other stuff here....
        ---
      end,
      sources = {
        -- Replace these with the tools you have installed
        null_ls.builtins.formatting.prettier,
        null_ls.builtins.diagnostics.eslint,
        null_ls.builtins.formatting.stylua,
      }
    })
  end, function()
      print("Failed to load lspconfig")
  end
  )
end

return M
