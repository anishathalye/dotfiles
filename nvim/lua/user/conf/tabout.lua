local status_ok, tabout = pcall(require, "tabout")
if not status_ok then
  vim.notify("tabout spellsitter not found")
	return
end

tabout.setup ({
  tabkey = "<A-l>",
  backwards_tabkey = "<A-h>",
  ignore_beginning = false,
  act_as_tab = true,
  enable_backward = true,
  completion = true,
  tabouts = {
  { open = "'", close = "'" },
  { open = '"', close = '"' },
  { open = "`", close = "`" },
  { open = "(", close = ")" },
  { open = "[", close = "]" },
  { open = "{", close = "}" },
  },
  exclude = {},
}
)
