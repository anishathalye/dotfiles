local M = {}

local function configure()
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

local function configure_ui()
  -- dapui config
  local dap, dapui = require "dap", require "dapui"
  dapui.setup {} -- use default
  dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
    dapui.close("tray")
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
  require("user.dap.dap-cpp")
  require ("user.dap.di-python")
  -- require("config.dap.python").setup()
  -- require("config.dap.rust").setup()
  -- require("config.dap.go").setup()
end

function M.setup()
  configure() -- Configuration
  configure_ui() -- config ui
  configure_debuggers() -- Debugger
end

return M
