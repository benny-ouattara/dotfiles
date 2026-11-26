-- Nvim 0.11+ owns LSP configuration: nvim-lspconfig now only ships `lsp/<name>.lua`
-- definitions, and mason-lspconfig calls vim.lsp.enable() for what's installed.
-- Server-specific overrides go through vim.lsp.config().

vim.lsp.config("*", {
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
})

vim.lsp.config("lua_ls", {
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim" },
      },
    },
  },
})
