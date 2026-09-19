
-- 1. Register core, UI, and Mason plugins as optional
vim.pack.add{{ src='https://github.com/williamboman/mason.nvim', type = 'opt' }}
vim.pack.add{{ src='https://github.com/jay-babu/mason-nvim-dap.nvim', type = 'opt' }}
vim.pack.add{{ src='https://github.com/nvim-neotest/nvim-nio', type = 'opt' }}
vim.pack.add{{ src='https://github.com/mfussenegger/nvim-dap', type = 'opt' }}
vim.pack.add{{ src='https://github.com/rcarriga/nvim-dap-ui', type = 'opt' }}
vim.pack.add{{ src='https://github.com/theHamsta/nvim-dap-virtual-text', type = 'opt' }}
vim.pack.add{{ src='https://github.com/leoluz/nvim-dap-go', type = 'opt' }}
vim.pack.add{{ src='https://github.com/mfussenegger/nvim-dap-python', type = 'opt' }}

-- 2. Centralized Lazy-Loading Function
local function load_dap_stack()
  if package.loaded["dap"] then return end

  -- Load Mason and Core plugins into the active runtime
  vim.cmd('packadd mason.nvim')
  vim.cmd('packadd mason-nvim-dap.nvim')
  vim.cmd('packadd nvim-nio')
  vim.cmd('packadd nvim-dap')
  vim.cmd('packadd nvim-dap-ui')
  vim.cmd('packadd nvim-dap-virtual-text')
  vim.cmd('packadd nvim-dap-go')
  vim.cmd('packadd nvim-dap-python')

  -- 3. Initialize Mason FIRST
  require("mason").setup()
  require("mason-nvim-dap").setup({
    -- Automatically install these adapters if not present
    ensure_installed = { 
      "delve",    -- Golang
      "debugpy",  -- Python
      "codelldb", -- Rust/C
      "netcoredbg", -- C#/F#
      "js-debug-adapter" -- TypeScript
    },
    automatic_installation = true,
  })

  -- 4. Standard DAP & UI Setup
  local dap = require("dap")
  local dapui = require("dapui")
  dapui.setup()
  require("nvim-dap-virtual-text").setup()

  -- UI Auto-toggle
  dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
  dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close() end
  dap.listeners.before.event_exited["dapui_config"] = function() dapui.close() end

  -- 5. Language Specific configurations
  require('dap-go').setup()

  -- Mason installs debugpy in a specific venv path
  require('dap-python').setup('~/.local/share/nvim/mason/packages/debugpy/venv/bin/python')

  -- Rust (codelldb)
  dap.adapters.codelldb = {
    type = 'server',
    port = "${port}",
    executable = {
      command = vim.fn.stdpath("data") .. "/mason/bin/codelldb",
      args = {"--port", "${port}"},
    }
  }

  -- .NET (C# / F#)
  dap.adapters.coreclr = {
    type = 'executable',
    command = vim.fn.stdpath("data") .. "/mason/bin/netcoredbg",
    args = {'--interpreter=vscode'}
  }
end

-- 6. Re-using the triggers from before
vim.keymap.set('n', '<F5>', function()
  load_dap_stack()
  require("dap").continue()
end, { desc = "Debug: Start/Continue" })

vim.keymap.set('n', '<leader>b', function()
  load_dap_stack()
  require("dap").toggle_breakpoint()
end, { desc = "Debug: Toggle Breakpoint" })

