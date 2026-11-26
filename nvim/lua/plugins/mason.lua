local options = {
  ui = {
    icons = {
      package_installed = " ",
      package_pending = " ",
      package_uninstalled = " ﮊ",
    },
  },
}

require("mason").setup(options)
