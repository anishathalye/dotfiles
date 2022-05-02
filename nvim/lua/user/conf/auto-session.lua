-- https://github.com/rmagatti/auto-session
local status_ok, auto_session = pcall(require, "auto-session")
if not status_ok then
  vim.notify("auto-session not found!")
  return
end

vim.o.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal"
auto_session.setup({
  -- 自动加载最后保存的一次会话
  auto_session_enable_last_session = false,
  pre_save_cmds = { "tabdo NvimTreeClose" }
}
)

-- 在每次退出 neovim 时自动保存会话
-- 其实该插件不加这个自动命令也能
-- 自动保存会话，但总是感觉效果不理想
-- 所以这里我就自己加了个自动命令
vim.cmd([[
    augroup _session
      autocmd! * <buffer>
      autocmd VimLeavePre * silent! :SaveSession
    augroup end
]])
