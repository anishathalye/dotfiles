local status_ok, nvim_startup = pcall(require, "nvim-startup")
if not status_ok then
  vim.notify("nvim_startup not found!")
	return
end

nvim_startup.setup{
  startup_file = '/tmp/nvim-startuptime', -- sets startup log path (string)
  message = 'Whoa! those {} are pretty fast', -- sets a custom message (string | function)
  -- message = function(time) -- function-based custom message
  --   time < 100 and 'Just {}? really good!' or 'Those {} can get faster'
  -- end,
}
