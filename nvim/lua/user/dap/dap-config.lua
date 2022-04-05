local M = {}

local function config_dapi_and_sign()
  local dap_install = require "dap-install"
  dap_install.setup {
    installation_path = vim.fn.stdpath "data" .. "/dapinstall/",
  }

  local dap_breakpoint = {
    error = {
      text = "🛑",
      texthl = "LspDiagnosticsSignError",
      linehl = "",
      numhl = "",
    },
    rejected = {
      text = "",
      texthl = "LspDiagnosticsSignHint",
      linehl = "",
      numhl = "",
    },
    stopped = {
      text = "⭐️",
      texthl = "LspDiagnosticsSignInformation",
      linehl = "DiagnosticUnderlineInfo",
      numhl = "LspDiagnosticsSignInformation",
    },
  }

  vim.fn.sign_define("DapBreakpoint", dap_breakpoint.error)
  vim.fn.sign_define("DapStopped", dap_breakpoint.stopped)
  vim.fn.sign_define("DapBreakpointRejected", dap_breakpoint.rejected)
end

local function config_dapui()
  -- dapui config
  local dap, dapui = require "dap", require "dapui"
  dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
    dapui.close("tray")
    -- dap.repl.close()
  end
  dap.listeners.before.event_terminated["dapui_config"] = function()
    dapui.close()
  end
  dap.listeners.before.event_exited["dapui_config"] = function()
    dapui.close()
  end
  -- for some debug adapter, terminate or exit events will no fire, use disconnect reuest instead
  dap.listeners.before.disconnect["dapui_config"] = function()
    dapui.close()
  end
  -- TODO: wait dap-ui for fix temrinal layout
  -- the "30" of "30vsplit: doesn't work
  dap.defaults.fallback.terminal_win_cmd = '30vsplit new' -- this will be override by dapui
end


local function configure_debuggers()
  -- load from json file
  require('dap.ext.vscode').load_launchjs(nil, {cppdbg = {'cpp'}})
  
  -- config per launage
  require("user.dap.dap-cpp")
  require ("user.dap.di-python")
  -- require("config.dap.python").setup()
  -- require("config.dap.rust").setup()
  -- require("config.dap.go").setup()
end

function M.setup()
  config_dapi_and_sign()
  config_dapui()
  configure_debuggers() -- Debugger
end

return M
