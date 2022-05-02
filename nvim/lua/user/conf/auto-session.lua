-- https://github.com/rmagatti/auto-session
local status_ok, auto_session = pcall(require, "auto-session")
if not status_ok then
  vim.notify("autos-ession not found!")
  return
end


vim.o.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal"
auto_session.setup({
  log_level = 'error',
  auto_session_root_dir = vim.fn.stdpath('data') .. "/sessions/",
  auto_session_enabled = true,
  auto_session_enable_last_session = false,
  auto_save_enabled = true,
  auto_restore_enabled = true,
  auto_session_suppress_dirs = nil,
  auto_session_use_git_branch = nil,
  pre_save_cmds = { "tabdo NvimTreeClose", },
  post_restore_cmds = { "NvimTreeFindFile" },
})

vim.cmd([[
    autocmd VimLeavePre * silent! :SaveSession
]])

