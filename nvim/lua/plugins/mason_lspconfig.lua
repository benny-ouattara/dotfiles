local options = {
  ensure_installed = { "pyright", "texlab", "clangd", "lua_ls" },
  -- automatic_enable defaults to true: installed servers are vim.lsp.enable()d.
}

require("mason-lspconfig").setup(options)
