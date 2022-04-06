-- https://github.com/folke/todo-comments.nvim
local status_ok, todo_comments = pcall(require, "todo-comments")
if not status_ok then
  vim.notify("todo-comments not found!")
	return
end

todo_comments.setup(
    {
        keywords = {
            -- alt ： 别名
            FIX = {
                icon = " ",
                color = "#DC2626",
                alt = {"FIXME", "BUG", "FIXIT", "ISSUE", "!"}
            },
            TODO = {icon = " ", color = "#2563EB"},
            HACK = {icon = " ", color = "#7C3AED"},
            WARN = {icon = " ", color = "#FBBF24", alt = {"WARNING", "XXX"}},
            PERF = {icon = " ", color = "#FC9868", alt = {"OPTIM", "PERFORMANCE", "OPTIMIZE"}},
            NOTE = {icon = " ", color = "#10B981", alt = {"INFO"}}
        }
    }
)

-- see whcih key 
-- vim.api.nvim_set_keymap("n", "<leader>td", "<cmd>TodoTelescope theme=dropdown<CR>", vim.keybinds.opts)











