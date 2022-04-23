vim.cmd([[
  let g:local_history_enabled=1     "0: Never (Disable local history plugin),1: Always (Save also a single file which is not in the workspace folder)2: Workspace (Save only files within workspace folder)
  let g:local_history_path="./.local_history"
  let g:local_history_width=30
  let g:local_history_new_change_delay=300    "5mins
  let g:local_history_mappings = {
    \ "move_older": ["j"],
    \ "move_newer": ["k"],
    \ "move_oldest": ["G"],
    \ "move_newest": ["gg"],
    \ "revert": ["<cr>"],
    \ "diff": ["d"],
    \ "delete": ["D"],
    \ "quit": ["q", "Q"],
    \ }
]])

-- move_older	Navigate to older change	j
-- move_newer	Navigate to newer change	k
-- move_oldest	Navigate to the oldest change	G
-- move_newest	Navigate to the newest change	gg
-- revert	Revert to selected change	Enter
-- diff	Vertical diff of current buffer with selected change	r
-- delete	Delete selected change	d
-- bigger	Increase local history graph size	L
-- smaller	Decrease local history graph size	H
-- preview_bigger	Increase local history preview size	K
-- preview_smaller	Decrease local history preview size	J
-- quit
