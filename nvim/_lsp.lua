-- fsharp
local util = require("lspconfig.util")

--[[
-- Print contents of `tbl`, with indentation.
-- `indent` sets the initial level of indentation.
local function tprint(tbl, indent)
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

vim.lsp.config["fsharp_fsautocomplete"] = {
    cmd = { "fsautocomplete", "--adaptive-lsp-server-enabled" },
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
}

-- Golang
vim.lsp.config["go_gopls"] = {
    cmd = { "gopls" },
    filetypes = { "go" },
    root_markers = { "go.mod", ".git" },
    settings = {},
}

-- Python
vim.lsp.config["python_ruff"] = {
    cmd = { "ruff", "server", "--preview" },
    filetypes = { "python" },
    single_file_support = true,
    root_markers = { "pyproject.toml", "ruff.toml", ".ruff.toml", ".git" },
    setup = {},
    settings = {
        python = {
            analysis = {
                typeCheckingMode = "basic",
            },
        },
    },
    on_attach = function(client, buffer)
        client.server_capabilities.documentFormattingProvider = true
        client.server_capabilities.hoverProvider = true
        client.server_capabilities.renameProvider = true
    end,
    -- settings = {},
}

-- Rust
vim.lsp.config["rust_rust_analyzer"] = {
    -- Server-specific settings. See `:help lsp-quickstart`
    cmd = { "rust_analyzer" },
    filetypes = { "rust" },
    root_markers = { "Cargo.toml" },
    settings = {},
}
