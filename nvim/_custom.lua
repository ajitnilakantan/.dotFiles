-- Add this to your `init.lua` or a custom file
-- require("conform").setup({
-- sources = {
-- require("conform").builtins.formatting.stylua.with({
-- extra_args = { "--indent-type", "Spaces", "--indent-width", "4" },
-- }),
-- },
-- })

if vim.fn.has("gui_running") == 0 then
    vim.o.termguicolors = false
    vim.cmd.colorscheme("gruvbox")
end

vim.lsp.enable("fsharp-fsautocomplete")
vim.lsp.enable("go-gopls")
vim.lsp.enable("python-ruff")
vim.lsp.enable("rust_analyzer")

-- Formatter args
local utils = require("conform.util")
---- Json
utils.add_formatter_args(require("conform.formatters.stylua"), { "--indent-type", "Spaces", "--indent-width", "4" })
