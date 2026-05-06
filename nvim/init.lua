-- vim:fileencoding=utf-8:foldmethod=marker
-- ~/.dotFiles/nvim/init.lua

-- 1. SETTINGS & OPTIONS

-- Default font.  Type :set guifont=*   to see available fonts
if vim.fn.has("gui_running") == 1 then
  -- Add :-calt to the font string to disable ligatures
  if vim.fn.has("mac") == 1 then
    vim.opt.guifont = "Menlo:h12::-calt"
  else
    vim.opt.guifont = "Consolas:h12::-calt"
  end
end

-- set <space> as the leader key
-- must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- General
vim.opt.number = true -- Line numbers
vim.opt.relativenumber = true -- Relative line numbers
vim.opt.cursorline = true -- Highlight current line

-- Indentation
vim.opt.tabstop = 4 -- Tab width
vim.opt.shiftwidth = 4 -- Indent width
vim.opt.softtabstop = 4 -- Soft tab stop
vim.opt.expandtab = true -- Use spaces instead of tabs
vim.opt.smartindent = true -- Smart auto-indenting
vim.opt.autoindent = true -- Copy indent from current line


-- Mouse
vim.opt.mouse = "a" -- Enable mouse support
vim.opt.clipboard = vim.env.SSH_TTY and "" or "unnamedplus" -- Sync with system clipboard

-- Gui
vim.opt.termguicolors = true
vim.opt.background = "dark"
vim.cmd.colorscheme("catppuccin")
vim.diagnostic.config({ underline = true })
vim.diagnostic.config({
  float = {
    border = "rounded", -- Options: "single", "double", "rounded", "solid", "shadow"
    -- This ensures the floating window matches your colorscheme
    winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:Visual",
  },
})


-- Visual settings
vim.opt.showmatch = true -- Highlight matching brackets
vim.opt.matchtime = 2 -- How long to show matching bracket
vim.opt.pumheight = 10 -- Popup menu height
vim.opt.pumblend = 10 -- Popup menu transparency
vim.opt.winblend = 0 -- Floating window transparency
vim.opt.winborder = 'rounded' -- Use rounded borders for floating windows.

-- Show whitespace.
vim.opt.list = true
vim.opt.listchars = { trail = '⋅', tab = '  ↦' }

-- File handling
vim.opt.backup = false -- Don't create backup files
vim.opt.writebackup = false -- Don't create backup before writing
vim.opt.swapfile = false -- Don't create swap files
vim.opt.undofile = true -- Persistent undo
vim.opt.undolevels = 10000
-- vim.opt.undodir = vim.fn.expand("~/.vim/undodir") -- Undo directory
vim.opt.updatetime = 300 -- Faster completion
vim.opt.timeoutlen = vim.g.vscode and 1000 or 300 -- Lower than default (1000) to quickly trigger which-key
vim.opt.ttimeoutlen = 0 -- Key code timeout
vim.opt.autoread = true -- Auto reload files changed outside vim
vim.opt.autowrite = true -- Auto save


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
    "n-v-c:block-blinkwait1000-blinkon500-blinkoff500",
    "i-ci-ve:ver25-blinkwait1000-blinkon500-blinkoff500",
    "r-cr:hor20",
    "o:hor50",
    -- "n-v-c-sm:block", "i-ci-ve:ver25", "r-cr-o:hor20", "t:block-blinkon500-blinkoff500-TermCursor",
}

-- Basic keymap
vim.keymap.set('n', 'Y', 'yy', { desc = 'Yank whole line' })

-- 2. PLUGIN MANAGEMENT (Using Neovim 0.12 built-in vim.pack)
-- Plugins are added to the runtimepath. You must have these installed
-- in your site/pack/vendor/start/ directory or managed via vim.pack.add()
---@diagnostic disable-next-line: redefined-local
local plugins = {
  "nvim-telescope/telescope.nvim",
  "folke/which-key.nvim",
  "nvim-lua/plenary.nvim", -- Required for Telescope
}

vim.pack.add(plugins, { confirm = false })

-- 3. WHICH-KEY CONFIGURATION
local wk = require("which-key")
wk.setup({
    spec = {
        { "<leader>s", group = "[S]earch", icon = { color = "green" } },
    },
    layout = {
        width = { min = 20, max = 50 }, -- minimum and maximum width of the columns
        spacing = 3,                    -- spacing between columns
        columns = 2,                    -- FORCE maximum 2 columns
    },
})
wk.add({
  { "<leader>f", group = "Find (Telescope)" },
  { "<leader>l", group = "LSP" },
})

-- 4. TELESCOPE CONFIGURATION
local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "Live Grep" })
vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "Buffers" })

-- INFO: better statusline
---@diagnostic disable-next-line: redefined-local
local plugins = {
  "nvim-lualine/lualine.nvim",
  'https://github.com/akinsho/bufferline.nvim',
  'https://github.com/nvim-tree/nvim-web-devicons'
}
vim.pack.add(plugins, { confirm = false })
require("lualine").setup({})
require("bufferline").setup({
  options = {
    close_command = "bdelete! %d", -- Click close icon to delete
    -- right_mouse_command = "bdelete! %d", -- Right click to delete
    -- Optional: show a close button
    buffer_close_icon = '\u{f00d}',
  }
})

vim.filetype.add({
  extension = {
    fs = 'fsharp',
    fsi = 'fsharp',
    fsx = 'fsharp',
  },
})
-- 5. Treesitter
---@diagnostic disable-next-line: redefined-local
local plugins = {
  "nvim-treesitter/nvim-treesitter",
}
vim.pack.add(plugins, { confirm = false })
-- Create an autocommand to run TSUpdate on install/update
vim.api.nvim_create_autocmd('User', {
  pattern = 'PackChanged',
  callback = function(args)
    -- Check if the changed plugin is nvim-treesitter
    if args.match == 'nvim-treesitter' then
      -- Run TSUpdate silently
      vim.cmd('TSUpdate')
    end
  end,
})

-- Tree-sitter (Added languages)
---@diagnostic disable-next-line: missing-fields
require("nvim-treesitter.config").setup({
  ensure_installed = {
    "lua",
    "vim",
    "vimdoc",
    "rust",
    "python",
    "go",
    "c_sharp",
    "fsharp", -- Added requested parsers
  },
  indent = { enable = true },
  auto_install = true,
  sync_install = false,
  ignore_install = {},
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
})

-- 5. LSP CONFIGURATION (0.12 Native Style)
-- Neovim 0.12 allows enabling servers directly if they are in your PATH
-- Example: enabling the Lua Language Server (lua_ls)
-- lsp servers we want to use and their configuration
-- see `:h lspconfig-all` for available servers and their settings
---@diagnostic disable-next-line: redefined-local
local plugins = {
  "neovim/nvim-lspconfig", -- Still helpful for default server definitions
  -- NOTE: if you'd rather install the lsps through your OS package manager you
  -- can delete the next three mason-related lines and their setup calls below.
  -- see `:h lsp-quickstart` for more details.
  "https://github.com/mason-org/mason.nvim", -- package manager
  "https://github.com/mason-org/mason-lspconfig.nvim", -- lspconfig bridge
  "https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim", -- auto installer
}
vim.pack.add(plugins, { confirm = false })
local lsp_servers = {
  lua_ls = {
    -- https://luals.github.io/wiki/settings/ | `:h nvim_get_runtime_file`
    Lua = {
      -- Recognize "vim" etc. as globals
      globals = { "vim", "describe", "it", "before_each", "after_each", "packer_plugins", "MiniTest" },
      -- Include Neovim runtime files
      workspace = { library = vim.api.nvim_get_runtime_file("lua", true) },
    },
  },
  clangd = {}, -- C
  csharp_ls = {}, -- C#
  fsautocomplete = {}, -- fsharp
  gopls = {}, -- golang
  rust_analyzer = {}, -- rust
  ty = {}, -- python
}
require("mason").setup()
require("mason-lspconfig").setup()
require("mason-tool-installer").setup({
  ensure_installed = vim.tbl_keys(lsp_servers),
})

-- configure each lsp server on the table
-- to check what clients are attached to the current buffer, use
-- `:checkhealth vim.lsp`. to view default lsp keybindings, use `:h lsp-defaults`.
for server, config in pairs(lsp_servers) do
  vim.lsp.config(server, {
    settings = config,

    -- only create the keymaps if the server attaches successfully
    on_attach = function(_, bufnr)
      vim.keymap.set("n", "grd", vim.lsp.buf.definition, { buffer = bufnr, desc = "vim.lsp.buf.definition()" })

      vim.keymap.set("n", "grf", vim.lsp.buf.format, { buffer = bufnr, desc = "vim.lsp.buf.format()" })
    end,
  })
end


-- Bold symbol under cursor, instead of default reverse video
vim.api.nvim_set_hl(0, "LspReferenceRead", { default = false, bold = true, cterm = vim.empty_dict() })
vim.api.nvim_set_hl(0, "LspReferenceText", { default = false, bold = true })
vim.api.nvim_set_hl(0, "LspReferenceWrite", { default = false, bold = true })


--AUTOCOMMANDS--
local config_augroup = vim.api.nvim_create_augroup("Config", { clear = true })

-- LSP Keybindings using Which-Key
vim.api.nvim_create_autocmd("LspAttach", {
  group = config_augroup,
  callback = function(event)
    local opts = { buffer = event.buf }
    wk.add({
      { "gd", vim.lsp.buf.definition, desc = "Go to Definition", opts },
      { "K", vim.lsp.buf.hover, desc = "Hover Docs", opts },
      { "<leader>lr", vim.lsp.buf.rename, desc = "Rename Symbol", opts },
      { "<leader>la", vim.lsp.buf.code_action, desc = "Code Action", opts },
      { "<leader>ld", vim.lsp.buf.definition, desc = "Go to Definition", opts },
      { "<leader>lR", vim.lsp.buf.references, desc = "Show References", opts },
      { "<leader>lr", vim.lsp.buf.rename, desc = "Rename Symbol", opts },
      { "<leader>lK", vim.lsp.buf.hover, desc = "Hover Docs", opts },
    })
  end,
})

-- go to last loc when opening a buffer
vim.api.nvim_create_autocmd("BufReadPost", {
  group = config_augroup,
  callback = function(event)
    local exclude = { "gitcommit" } -- don't remember position in commit messages
    local buf = event.buf
    if vim.tbl_contains(exclude, vim.bo[buf].filetype) then
      return
    end
    local mark = vim.api.nvim_buf_get_mark(buf, '"')
    local lcount = vim.api.nvim_buf_line_count(buf)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- Highlight on yank: briefly highlights on yank for visual feedback
vim.api.nvim_create_autocmd("TextYankPost", {
  group = config_augroup,
  callback = function()
    (vim.hl or vim.highlight).on_yank()
  end,
})

-- Show errors and warnings in a floating window
vim.api.nvim_create_autocmd("CursorHold", {
  group = config_augroup,
  callback = function()
    vim.diagnostic.open_float(nil, { focusable = false, source = "if_many" })
  end,
})

-- Enable treesitter
vim.api.nvim_create_autocmd('FileType', {
  group = config_augroup,
  pattern = { '<filetype>' },
  callback = function() vim.treesitter.start() end,
})
