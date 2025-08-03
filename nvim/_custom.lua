--[[
-- Print contents of `tbl`, with indentation.
-- `indent` sets the initial level of indentation.
function tprint(tbl, indent)
    if not indent then
        indent = 0
    end
    for k, v in pairs(tbl) do
        local formatting = string.rep("  ", indent) .. k .. ": "
        if type(v) == "table" then
            print(formatting)
            tprint(v, indent + 1)
        else
            print(formatting .. tostring(v))
        end
    end
end
--]]

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

-- Blink cursor
vim.opt.guicursor = {
    "n-v-c:block-Cursor/lCursor-blinkwait1000-blinkon300-blinkoff300",
    "i-ci:ver25-Cursor/lCursor-blinkwait1000-blinkon300-blinkoff300",
    "r:hor50-Cursor/lCursor-blinkwait1000-blinkon300-blinkoff300",
}

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

local scriptFolder = vim.fn.fnamemodify(vim.fn.resolve(vim.fn.expand("<sfile>:p")), ":h")
local Util = dofile(scriptFolder .. "/nvim/_util.lua")

local wk = require("which-key")
wk.setup({ delay = 500 }) -- 500ms delay before showing help

vim.keymap.set("n", "<leader>c", builtin.colorscheme, { desc = "Telescope colorscheme" })
vim.keymap.set("n", "<leader><space>", "<Right>", { desc = "Forward" })

wk.add({
    { "<leader>l", group = "LSP commands" }, -- group
    { "<leader>lk", hoverfn, desc = "Hover info", mode = "n" }, -- group
    { "<leader>li", toggleInlays, desc = "Toggle inlay hints", mode = "n" }, -- group
    { "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>", desc = "Code action", mode = "n" }, -- group
})

vim.keymap.set("n", "K", hoverfn, { desc = "HoverInfo" })
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

-- Tab autocomplete
