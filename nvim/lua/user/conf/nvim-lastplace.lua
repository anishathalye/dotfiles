local status_ok, nvim_lastposition = pcall(require, "nvim-lastplace")
if not status_ok then
  vim.notify("nvim_lastposition not found!")
	return
end

nvim_lastposition.setup({
        lastplace_ignore_buftype = {"quickfix", "nofile", "help"},
        lastplace_ignore_filetype = {"gitcommit", "gitrebase", "svn", "hgcommit"},
        lastplace_open_folds = true
  }
)
