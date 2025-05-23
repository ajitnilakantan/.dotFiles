-- Adapted from: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/util/init.lua

---@type table
local Util = vim.tbl_extend("error", require("lazy.core.util"), {})

Util.root_patterns = { ".git", "lua" }

---@param str string
function Util.capcase(str)
    return str:sub(1, 1):upper() .. str:sub(2)
end

---@param str string
---@param group string
function Util.format_highlight(str, group)
    return "%#" .. group .. "#" .. str .. "%*"
end

function Util.close_floats()
    for _, win in ipairs(vim.api.nvim_list_wins()) do
        if vim.api.nvim_win_get_config(win).relative == "win" then
            vim.api.nvim_win_close(win, false)
        end
    end
end

--- Gets the buffer number of every visible buffer
--- @return integer[]
function Util.visible_buffers()
    return vim.tbl_map(vim.api.nvim_win_get_buf, vim.api.nvim_list_wins())
end

function Util.lsp_active()
    for _, client in pairs(vim.lsp.get_clients()) do
        if client.server_capabilities then
            return true
        end
    end
    return false
end

function Util.clear_highlights()
    vim.cmd("nohlsearch")
    if Util.lsp_active() then
        vim.lsp.buf.clear_references()
        for _, buffer in pairs(Util.visible_buffers()) do
            vim.lsp.util.buf_clear_references(buffer)
        end
    end
end

---@param str string
function Util.termcodes(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

---@param keys string
---@param mode? string
function Util.feedkeys(keys, mode)
    if mode == nil then
        mode = "in"
    end
    return vim.api.nvim_feedkeys(Util.termcodes(keys), mode, true)
end

---@param cb fun(client, buffer)
function Util.on_attach(cb)
    vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
            local buffer = args.buf
            local client = vim.lsp.get_client_by_id(args.data.client_id)
            cb(client, buffer)
        end,
    })
end

---@param plugin string
function Util.has(plugin)
    return require("lazy.core.config").plugins[plugin] ~= nil
end

---@param name string
function Util.opts(name)
    local plugin = require("lazy.core.config").plugins[name]
    if not plugin then
        return {}
    end
    local Plugin = require("lazy.core.plugin")
    return Plugin.values(plugin, "opts", false)
end

-- returns the root directory based on:
-- * lsp workspace folders
-- * lsp root_dir
-- * root pattern of filename of the current buffer
-- * root pattern of cwd
---@return string
function Util.get_root()
    ---@type string?
    local path = vim.api.nvim_buf_get_name(0)
    path = path ~= "" and vim.uv.fs_realpath(path) or nil
    ---@type string[]
    local roots = {}
    if path then
        for _, client in pairs(vim.lsp.get_clients({ bufnr = 0 })) do
            local workspace = client.config.workspace_folders
            local paths = workspace
                    and vim.tbl_map(function(ws)
                        return vim.uri_to_fname(ws.uri)
                    end, workspace)
                or client.config.root_dir and { client.config.root_dir }
                or {}
            for _, p in ipairs(paths) do
                local r = vim.uv.fs_realpath(p)
                if r ~= nil and path:find(r, 1, true) then
                    roots[#roots + 1] = r
                end
            end
        end
    end
    table.sort(roots, function(a, b)
        return #a > #b
    end)
    ---@type string?
    local root = roots[1]
    if not root then
        path = path and vim.fs.dirname(path) or vim.loop.cwd()
        ---@type string?
        root = vim.fs.find(Util.root_patterns, { path = path, upward = true })[1]
        root = root and vim.fs.dirname(root) or vim.loop.cwd()
    end
    ---@cast root string
    return root
end

---@param option string
---@param silent? boolean?
---@param values? {[1]:any, [2]:any}
function Util.toggle(option, silent, values)
    if values then
        if vim.opt_local[option]:get() == values[1] then
            vim.opt_local[option] = values[2]
        else
            vim.opt_local[option] = values[1]
        end
        return Util.info("Set " .. option .. " to " .. vim.opt_local[option]:get(), { title = "Option" })
    end
    vim.opt_local[option] = not vim.opt_local[option]:get()
    if not silent then
        if vim.opt_local[option]:get() then
            Util.info("Enabled " .. option, { title = "Option" })
        else
            Util.warn("Disabled " .. option, { title = "Option" })
        end
    end
end

function Util.toggle_diagnostics()
    if not vim.diagnostic.is_enabled() then
        vim.diagnostic.enable()
        Util.info("Enabled diagnostics", { title = "Diagnostics" })
    else
        vim.diagnostic.enable(false)
        Util.warn("Disabled diagnostics", { title = "Diagnostics" })
    end
end

function Util.service_status()
    local buf_clients = vim.lsp.get_clients()
    local buf = vim.api.nvim_get_current_buf()
    local buf_ft = vim.bo.filetype

    ---@class ServiceStatus
    ---@field diagnostic_providers string[]
    ---@field formatting_providers string[]
    ---@field copilot_active boolean
    ---@field treesitter_active boolean
    ---@field session_active boolean
    ---@field lazy_updates boolean
    local status = {
        diagnostic_providers = {},
        formatting_providers = {},
        copilot_active = false,
        treesitter_active = next(vim.treesitter.highlighter.active[buf]),
        session_active = vim.g.persisting == 1,
        lazy_updates = require("lazy.status").has_updates(),
        -- TODO: check for mason updates
        -- mason_updates
    }

    -- add lsp clients
    for _, client in pairs(buf_clients) do
        if client.name ~= "null-ls" and client.name ~= "copilot" then
            table.insert(status.diagnostic_providers, client.name)
        end
        if client.name == "copilot" then
            status.copilot_active = true
        end
    end

    -- add null-ls sources
    local _, sources = pcall(require, "null-ls.sources")
    if sources then
        local methods = require("null-ls").methods

        -- add formatter
        for _, formatter in pairs(sources.get_available(buf_ft, methods.FORMATTING)) do
            table.insert(status.formatting_providers, formatter.name)
        end

        -- add linter/diagnostics
        for _, linter in pairs(sources.get_available(buf_ft, methods.DIAGNOSTICS)) do
            table.insert(status.diagnostic_providers, linter.name)
        end
    end

    ---@class ServiceStatus
    return status
end

return Util
