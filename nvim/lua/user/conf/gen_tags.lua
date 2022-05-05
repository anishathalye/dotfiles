-- Ctrl+\ c    Find functions calling this function
-- Ctrl+\ d    Find functions called by this function
-- Ctrl+\ e    Find this egrep pattern
-- Ctrl+\ f    Find this file
-- Ctrl+\ g    Find this definition
-- Ctrl+\ i    Find files #including this file
-- Ctrl+\ s    Find this C symbol
-- Ctrl+\ t    Find this text string

vim.cmd(
[[
  let g:loaded_gentags#ctags = 1
  let g:gen_tags#gtags_default_map = 0
]]
)
