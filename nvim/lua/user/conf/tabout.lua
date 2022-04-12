local status_ok, tabout = pcall(require, "tabout")
if not status_ok then
  vim.notify("tabout not found")
  return
end

-- Lua
tabout.setup {
  tabkey = '<A-l>', -- key to trigger tabout, set to an empty string to disable
  backwards_tabkey = '<A-h>', -- key to trigger backwards tabout, set to an empty string to disable
  act_as_tab = true, -- shift content if tab out is not possible
  act_as_shift_tab = false, -- reverse shift content if tab out is not possible (if your keyboard/terminal supports <S-Tab>)
  enable_backwards = true, -- well ...
  completion = true, -- if the tabkey is used in a completion pum
  tabouts = {
    { open = "'", close = "'" },
    { open = '"', close = '"' },
    { open = '`', close = '`' },
    { open = '(', close = ')' },
    { open = '[', close = ']' },
    { open = '{', close = '}' }
  },
  ignore_beginning = true, --[[ if the cursor is at the beginning of a filled element it will rather tab out than shift the content ]]
  exclude = {} -- tabout will ignore these filetypes
}
