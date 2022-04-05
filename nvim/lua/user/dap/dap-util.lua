local M = {}
local dap =  require'dap'

function M.reload_continue()
  package.loaded['user.dap.dap-config'] = nil
  require('user.dap.dap-config').setup()
  dap.continue()
end

return M
