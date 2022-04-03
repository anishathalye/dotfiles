-- configure the litee.nvim library 
require('litee.lib').setup({
   panel = {
        orientation = "right",
        panel_size  = 50
    },
})

-- configure litee-calltree.nvim
require('litee.calltree').setup({
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
