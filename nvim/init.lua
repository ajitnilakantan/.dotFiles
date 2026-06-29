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
vim.opt.relativenumber = false -- Relative line numbers
vim.opt.cursorline = true -- Highlight current line
vim.opt.ignorecase = true -- Required for smartcase
vim.opt.smartcase = true -- Smartcase search


-- Indentation
vim.opt.tabstop = 4 -- Tab width
vim.opt.shiftwidth = 4 -- Indent width
vim.opt.softtabstop = 4 -- Soft tab stop
vim.opt.expandtab = true -- Use spaces instead of tabs
vim.opt.smartindent = true -- Smart auto-indenting
vim.opt.autoindent = true -- Copy indent from current line
vim.opt.textwidth = 0 -- Prevent automatic line break insertion
vim.opt.formatoptions:remove("c") -- Prevent auto line break for comments
vim.opt.formatoptions:remove("t")

-- Mouse
vim.opt.mouse = "a" -- Enable mouse support
vim.opt.clipboard = vim.env.SSH_TTY and "" or "unnamedplus" -- Sync with system clipboard

-- Gui
vim.opt.termguicolors = true
vim.opt.background = "dark"
vim.pack.add { { src = "https://github.com/catppuccin/nvim", name = "catppuccin" } }
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
vim.opt.winborder = "rounded" -- Use rounded borders for floating windows.

-- Show whitespace.
vim.opt.list = true
vim.opt.listchars = { trail = "⋅", tab = "  ↦" }

-- Fuzzy help
vim.opt.wildoptions:append("fuzzy")

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
vim.g.neovide_cursor_animation_length = 0
vim.g.neovide_cursor_trail_size = 0
vim.g.neovide_cursor_animate_in_insert_mode = false
vim.g.neovide_cursor_animate_command_line = false
vim.g.neovide_scroll_animation_far_lines = 0
vim.g.neovide_scroll_animation_length = 0
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
vim.keymap.set("n", "Y", "yy", { desc = "Yank whole line" })

-- Delete and change without cutting
vim.keymap.set({'n', 'v'}, 's', '"_s')
vim.keymap.set({'n', 'v'}, 'c', '"_c')
vim.keymap.set({'n', 'v'}, 'C', '"_C')

-- 2. PLUGIN MANAGEMENT (Using Neovim 0.12 built-in vim.pack)
-- Plugins are added to the runtimepath. You must have these installed
-- in your site/pack/vendor/start/ directory or managed via vim.pack.add()
---@diagnostic disable-next-line: redefined-local
local plugins = {
  "https://github.com/nvim-telescope/telescope.nvim",
  "https://github.com/folke/which-key.nvim",
  "https://github.com/nvim-lua/plenary.nvim", -- Required for Telescope
}

vim.pack.add(plugins, { confirm = false })

-- 3 WHICH-KEY CONFIGURATION
local wk = require("which-key")
wk.setup({
{
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      -- Choose layout preset: "classic", "modern", or "helix"
      preset = "modern", 
      -- Your custom groupings go here
      spec = {
        { "<leader>f", group = "Find/File" },
        { "<leader>g", group = "Git" },
        { "<leader>b", group = "Buffers" },
      },
    },
    keys = {
      {
        "<leader>?",
        function()
          require("which-key").show({ global = false })
        end,
        desc = "Buffer Local Keymaps",
      },
    },
  }
})

-- wk.setup({
--   spec = {
--     -- { "<leader>?", group = "[S]earch", icon = { color = "green" } },
--     { "<leader>?", group = "[S]earch" },
--   },
--   layout = {
--     width = { min = 20, max = 50 }, -- minimum and maximum width of the columns
--     spacing = 3, -- spacing between columns
--     columns = 2, -- FORCE maximum 2 columns
--   },
-- })
wk.add({
  { "<leader>f", group = "Find (Telescope)" },
  { "<leader>l", group = "LSP" },
})

-- 4. TELESCOPE CONFIGURATION
local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "Live Grep" })
vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "Buffers" })
vim.keymap.set("n", "<leader>vh", ":Telescope help_tags<CR>", { desc = "Fuzzy Help" })

-- INFO: better statusline
---@diagnostic disable-next-line: redefined-local
local plugins = {
  "https://github.com/nvim-lualine/lualine.nvim",
  "https://github.com/akinsho/bufferline.nvim",
  "https://github.com/nvim-tree/nvim-web-devicons",
}
vim.pack.add(plugins, { confirm = false })
require("lualine").setup({})
require("bufferline").setup({
  options = {
    close_command = "bdelete! %d", -- Click close icon to delete
    -- right_mouse_command = "bdelete! %d", -- Right click to delete
    -- Optional: show a close button
    buffer_close_icon = "\u{f00d}",
  },
})

vim.filetype.add({
  extension = {
    fs = "fsharp",
    fsi = "fsharp",
    fsx = "fsharp",
  },
})
-- 5. Treesitter
---@diagnostic disable-next-line: redefined-local
local plugins = {
  "https://github.com/nvim-treesitter/nvim-treesitter",
}
vim.pack.add(plugins, { confirm = false })
-- Create an autocommand to run TSUpdate on install/update
vim.api.nvim_create_autocmd("User", {
  pattern = "PackChanged",
  callback = function(args)
    -- Check if the changed plugin is nvim-treesitter
    if args.match == "nvim-treesitter" then
      -- Run TSUpdate silently
      vim.cmd("TSUpdate")
    end
  end,
})

-- Tree-sitter (Added languages)
-- 1. Initialize the tree-sitter plugin path configurations
-- require("nvim-treesitter").setup({})

vim.api.nvim_create_autocmd('FileType', {
  callback = function(ev)
    local lang = vim.treesitter.language.get_lang(ev.match)
    local ts = require('nvim-treesitter')

    -- Check if the parser is valid and available via nvim-treesitter
    if vim.tbl_contains(ts.get_available(), lang) then
      -- If the parser is not yet installed, download it synchronously
      if not vim.tbl_contains(ts.get_installed(), lang) then
        ts.install(lang):wait()
      end
      -- Start the Treesitter highlighting engine
      vim.treesitter.start()
      vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()" -- Enables native folding
      vim.wo.foldmethod = "expr"
      vim.opt.foldenable = false -- Initially unfolded
    end
  end,
})
-- -- 2. Define your target languages (your old ensure_installed list)
-- local languages = { "lua", "vim", "vimdoc", "c_sharp", "fsharp", "go", "python", "rust", "javascript" }
-- 
-- -- 3. Download the parsers asynchronously on startup
-- require("nvim-treesitter").install(languages)
-- 
-- -- 4. Enable native Neovim features for these languages automatically
-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = languages,
--   callback = function()
--     vim.treesitter.start() -- Turns on native syntax highlighting
--     vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()" -- Enables native folding
--     vim.wo.foldmethod = "expr"
--   end,
-- })

--  ---@diagnostic disable-next-line: missing-fields
--  require("nvim-treesitter.config").setup({
--    ensure_installed = {
--      "lua",
--      "vim",
--      "vimdoc",
--      "rust",
--      "python",
--      "go",
--      "c_sharp",
--      "fsharp", -- Added requested parsers
--    },
--    indent = { enable = true },
--    auto_install = true,
--    sync_install = false,
--    ignore_install = {},
--    highlight = {
--      enable = true,
--      additional_vim_regex_highlighting = false,
--    },
--  })

-- 5. LSP CONFIGURATION (0.12 Native Style)
-- Neovim 0.12 allows enabling servers directly if they are in your PATH
-- Example: enabling the Lua Language Server (lua_ls)
-- lsp servers we want to use and their configuration
-- see `:h lspconfig-all` for available servers and their settings

--- Merge two key-value tables
---@param t1 table
---@param t2 table
---@diagnostic disable-next-line: unused-function
local function mergeTables(t1, t2)
  local result = {}

  -- Copy t1 first
  for k, v in pairs(t1) do
    if type(v) == "table" then
      result[k] = mergeTables({}, v) -- Clone sub-tables
    else
      result[k] = v
    end
  end

  -- Merge t2 into the result
  for k, v in pairs(t2) do
    if type(v) == "table" and type(result[k]) == "table" then
      result[k] = mergeTables(result[k], v) -- Recursively merge nested tables
    else
      result[k] = v
    end
  end

  return result
end
-- local user = { name = "Sam", age = 20 }
-- local extra = { age = 21, city = "Seattle" }
-- mergeTables(user, extra) -- Result: { name = "Sam", age = 21, city = "Seattle" }

---Recursively print the table; if the value is not a table, then just print the value.
---@param t any
---@param level? number
---@diagnostic disable-next-line: unused-function
local function printTable(t, level)
  level = level or 0
  if type(t) == "table" then
    io.write("{\n")
    for key, value in pairs(t) do
      io.write(string.rep("\t", level) .. string.format("[%s] = ", key))
      printTable(value, level)
      io.write(",\n")
    end
    io.write("}\n")
  else
    io.write(tostring(t))
  end
end

---@diagnostic disable-next-line: redefined-local
local plugins = {
  "https://github.com/neovim/nvim-lspconfig", -- Still helpful for default server definitions
  -- NOTE: if you'd rather install the lsps through your OS package manager you
  -- can delete the next three mason-related lines and their setup calls below.
  -- see `:h lsp-quickstart` for more details.
  "https://github.com/mason-org/mason.nvim", -- package manager
  "https://github.com/mason-org/mason-lspconfig.nvim", -- lspconfig bridge
  "https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim", -- auto installer
}
vim.pack.add(plugins, { confirm = false })
local util = require("lspconfig.util")
local lsp_servers = {
  lua_ls = {
    settings = {
      -- https://luals.github.io/wiki/settings/ | `:h nvim_get_runtime_file`
      Lua = {
        -- Recognize "vim" etc. as globals
        globals = { "vim", "describe", "it", "before_each", "after_each", "packer_plugins", "MiniTest" },
        -- Include Neovim runtime files
        workspace = { library = vim.api.nvim_get_runtime_file("lua", true) },
      },
    },
  },
  clangd = {}, -- C
  csharp_ls = {}, -- C#
  fsautocomplete = {
    cmd = { "fsautocomplete", "--adaptive-lsp-server-enabled", "--use-fcs-transparent-compiler" },
    root_dir = function(bufnr, on_dir)
      local fname = vim.api.nvim_buf_get_name(bufnr)
      on_dir(util.root_pattern("*.sln", "*.fsproj", ".git")(fname))
    end,
    filetypes = { "fsharp" },
    init_options = {
      AutomaticWorkspaceInit = true,
    },
    -- this recommended settings values taken from  https://github.com/ionide/FsAutoComplete?tab=readme-ov-file#settings
    settings = {
      FSharp = {
        keywordsAutocomplete = true,
        ExternalAutocomplete = false,
        Linter = true,
        UnionCaseStubGeneration = true,
        UnionCaseStubGenerationBody = 'failwith "Not Implemented"',
        RecordStubGeneration = true,
        RecordStubGenerationBody = 'failwith "Not Implemented"',
        InterfaceStubGeneration = true,
        InterfaceStubGenerationObjectIdentifier = "this",
        InterfaceStubGenerationMethodBody = 'failwith "Not Implemented"',
        UnusedOpensAnalyzer = true,
        UnusedDeclarationsAnalyzer = true,
        UseSdkScripts = true,
        SimplifyNameAnalyzer = true,
        ResolveNamespaces = true,
        EnableReferenceCodeLens = true,
      },
    },
  }, -- fsharp
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
  vim.lsp.config(
    server,
    mergeTables(config, {
      -- only create the keymaps if the server attaches successfully
      on_attach = function(_, bufnr)
        vim.keymap.set("n", "grd", vim.lsp.buf.definition, { buffer = bufnr, desc = "vim.lsp.buf.definition()" })

        vim.keymap.set("n", "grf", vim.lsp.buf.format, { buffer = bufnr, desc = "vim.lsp.buf.format()" })
      end,
    })
  )
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

-- Workaround for bug https://github.com/neovim/neovim/issues/36257
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client and client.server_capabilities then
      client.server_capabilities.semanticTokensProvider = nil
    end
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

-- -- Enable treesitter
-- vim.api.nvim_create_autocmd("FileType", {
--   group = config_augroup,
--   pattern = { "<filetype>" },
--   callback = function()
--     vim.treesitter.start()
--   end,
-- })
