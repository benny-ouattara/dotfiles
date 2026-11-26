-- indent-blankline v3 ("ibl"); the v2 `indent_blankline` API is gone.
require("ibl").setup {
  indent = {
    char = "│",
  },
  whitespace = {
    remove_blankline_trail = false,
  },
  scope = {
    enabled = true,
    show_start = false,
    show_end = false,
  },
  exclude = {
    filetypes = {
      "help",
      "terminal",
      "lspinfo",
      "TelescopePrompt",
      "TelescopeResults",
      "lazy",
      "mason",
      "",
    },
    buftypes = { "terminal" },
  },
}
