local colorscheme = "catppuccin"
-- require "user.conf.github-nvim-theme"

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
  vim.notify("colorscheme " .. colorscheme .. " not found!")
  return
end

if colorscheme == "onedark" then
  require "user.conf.onedark"
end
