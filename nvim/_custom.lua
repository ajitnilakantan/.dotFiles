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

-- Make matchParen less intrusive
vim.cmd([[hi MatchParen cterm=underline,bold ctermbg=none]])
vim.cmd([[hi clear CursorLine]])

-- Bold symbol under cursor, instead of default reverse video
vim.api.nvim_set_hl(0, "LspReferenceRead", { default = false, bold = true, cterm = vim.empty_dict() })
vim.api.nvim_set_hl(0, "LspReferenceText", { default = false, bold = true })
vim.api.nvim_set_hl(0, "LspReferenceWrite", { default = false, bold = true })

-- NeoVim Annoyances
vim.g.neovide_position_animation_length = 0
vim.g.neovide_cursor_animation_length = 0.05
vim.g.neovide_cursor_trail_size = 0
vim.g.neovide_cursor_animate_in_insert_mode = false
vim.g.neovide_cursor_animate_command_line = false
vim.g.neovide_scroll_animation_far_lines = 0
vim.g.neovide_scroll_animation_length = 0.0
vim.o.mousescroll = "ver:3,hor:0"

-- keymaps
local builtin = require("telescope.builtin")

local hoverfn = function()
    vim.lsp.buf.hover({
        border = "rounded",
    })
end
local toggleInlays = function()
    vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
end

Util = require("_util")

vim.keymap.set("n", "<leader>c", builtin.colorscheme, { desc = "Telescope colorscheme" })
vim.keymap.set("n", "<leader><space>", "<Right>", { desc = "Forward" })
vim.keymap.set("n", "<leader>k", hoverfn, { desc = "HoverInfo" })
vim.keymap.set("n", "K", hoverfn, { desc = "HoverInfo" })
vim.keymap.set("n", "<leader>i", toggleInlays, { desc = "Toggle inlay hints" })
vim.keymap.set("n", "<Esc>", function()
    Util.close_floats()
    if vim.bo.modifiable then
        Util.clear_highlights()
    else
        if #vim.api.nvim_list_wins() > 1 then
            return Util.feedkeys("<C-w>c")
        end
    end
end, { desc = "Close floats, clear highlights" })

vim.lsp.enable("fsharp_fsautocomplete")
vim.lsp.enable("go_gopls")
vim.lsp.enable("python_ruff")
vim.lsp.enable("rust_analyzer")

vim.lsp.enable("pyright")

-- Formatter args
local utils = require("conform.util")
-- Use space instead of tabs for Lua autoformatter
utils.add_formatter_args(require("conform.formatters.stylua"), { "--indent-type", "Spaces", "--indent-width", "4" })
