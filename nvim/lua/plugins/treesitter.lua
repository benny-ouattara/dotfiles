-- nvim-treesitter `main` installs parsers and ships queries; enabling features
-- is Neovim's job, so highlighting has to be turned on per filetype.
local languages = { "c", "cpp", "lua", "python", "vim" }

require("nvim-treesitter").install(languages)

vim.api.nvim_create_autocmd("FileType", {
  pattern = languages,
  callback = function()
    pcall(vim.treesitter.start)
  end,
})
