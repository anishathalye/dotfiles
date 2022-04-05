local dap_install = require("dap-install")
dap_install.config(
  "ccppr_vsc",
{
    -- adapters = {
    --   type = "executable",
    --   command = os.getenv('HOME') .. '/.config/nvim/lua/user/dap/debugger/ms-vscode.cpptools-1.7.1/debugAdapters/bin/OpenDebugAD7',
    --   -- command = "/home/raven/.local/share/nvim/dapinstall/ccppr_vsc/extension/debugAdapters/bin/OpenDebugAD7",
    -- },
    -- configurations = {
    -- {
    --     name = "Launch file",
    --     type = "cpptools",
    --     request = "launch",
    --     miDebuggerPath = "/usr/bin/gdb",
    --     program = function()
    --       return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
    --     end,
    --     cwd = "${workspaceFolder}",
    --     stopOnEntry = true,
    -- },
    -- {
    --     name = "Attach process",
    --     type = "cpptools",
    --     request = "attach",
    --     miDebuggerPath = "/usr/bin/gdb",
    --     program = function()
    --       return require("dap.utils.pick_process").pick_process()
    --     end,
    --     cwd = "${workspaceFolder}",
    --     stopOnEntry = true,
    --
    -- },
    -- {
    --     name = "Attach to gdbserver :1234",
    --     type = "cppdbg",
    --     request = "launch",
    --     MIMode = "gdb",
    --     miDebuggerServerAddress = "localhost:1234",
    --     miDebuggerPath = "/usr/bin/gdb",
    --     cwd = "${workspaceFolder}",
    --     program = function()
    --       return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
    --     end,
    --   },
    -- },
  }
)
