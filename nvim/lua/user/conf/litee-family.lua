-- configure the litee.nvim library 
local status_ok, litee_lib = pcall(require, "litee.lib")
if not status_ok then
  vim.notify("litee_lib not found!")
  return
end

local status_ok, litee_calltree = pcall(require, "litee.calltree")
if not status_ok then
  vim.notify("litee_calltree not found!")
  return
end

litee_lib.setup({
   panel = {
        orientation = "right",
        panel_size  = 50
    },
})

-- configure litee-calltree.nvim
-- commands: LTOpenToCalltree to open calltree
litee_calltree.setup({
   -- NOTE: the plugin is in-progressing
  on_open = "pannel", -- pannel | popout
  hide_cursor = false,
  keymaps = {
    expand = "o",
    collapse = "zc",
    collapse_all = "zM",
    jump = "<CR>",
    jump_split = "s",
    jump_vsplit = "v",
    jump_tab = "t",
    hover = "i",
    details = "d",
    close = "X",
    close_panel_pop_out = "<C-c>",
    help = "?",
    hide = "H",
    switch = "S",
    focus = "f"
  },
})
