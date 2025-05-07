if vim.fn.has("mac") == 1 then
    if vim.fn.has("gui_running") == 0 then
        vim.o.termguicolors = false
        vim.cmd.colorscheme("gruvbox")
    end
end

if vim.fn.has("windows") == 1 then
    if vim.fn.has("gui_running") == 0 then
        vim.cmd.colorscheme("darkblue")
    end
end

-- Annoyances
vim.g.neovide_position_animation_length = 0
vim.g.neovide_cursor_animation_length = 0.0
vim.g.neovide_cursor_trail_size = 0
vim.g.neovide_cursor_animate_in_insert_mode = false
vim.g.neovide_cursor_animate_command_line = false
vim.g.neovide_scroll_animation_far_lines = 0
vim.g.neovide_scroll_animation_length = 0.0
vim.o.mousescroll = "ver:3,hor:0"

-- keymaps
local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>c", builtin.colorscheme, { desc = "Telescope colorscheme" })
vim.keymap.set("n", "<leader><space>", "<Right>", { desc = "Forward" })

vim.lsp.enable("fsharp_fsautocomplete")
vim.lsp.enable("go_gopls")
vim.lsp.enable("python_ruff")
vim.lsp.enable("rust_analyzer")

-- Formatter args
local utils = require("conform.util")
---- Use space instead of tabs for Lua autoformatter
utils.add_formatter_args(require("conform.formatters.stylua"), { "--indent-type", "Spaces", "--indent-width", "4" })
