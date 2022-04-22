-- NOTE: install ripgrep for live_grep picker

-- ====for live_grep raw====:
-- for rp usage: reference: https://segmentfault.com/a/1190000016170184
-- -i ignore case
-- -s 大小写敏感
-- -w match word
-- -e 正则表达式匹配
-- -v 反转匹配
-- -g 通配符文件或文件夹，可以用!来取反
-- -F fixed-string 原意字符串，类似python的 r'xxx'

-- examples:
-- command	Description
-- rg image utils.py	Search in a single file utils.py
-- rg image src/	Search in dir src/ recursively
-- rg image	Search image in current dir recursively
-- rg '^We' test.txt	Regex searching support (lines starting with We)
-- rg -i image	Search image and ignore case (case-insensitive search)
-- rg -s image	Smart case search
-- rg -F '(test)'	Search literally, i.e., without using regular expression
-- rg image -g '*.py'	File globing (search in certain files), can be used multiple times
-- rg image -g '!*.py'	Negative file globing (do not search in certain files)
-- rg image --type py or rg image -tpy1	Search image in Python file
-- rg image -Tpy	Do not search image in Python file type
-- rg -l image	Only show files containing image (Do not show the lines)
-- rg --files-without-match image	Show files not containing image
-- rg -v image	Inverse search (search files not containing image)
-- rg -w image	Search complete word
-- rg --count	Show the number of matching lines in a file
-- rg --count-matches	Show the number of matchings in a file
-- rg neovim --stats	Show the searching stat (how many matches, how many files searched etc.)

-- ====for fzf search=====
-- Token	Match type	Description
-- sbtrkt	fuzzy-match	Items that match sbtrkt
-- 'wild	exact-match (quoted)	Items that include wild
-- ^music	prefix-exact-match	Items that start with music
-- .mp3$	suffix-exact-match	Items that end with .mp3
-- !fire	inverse-exact-match	Items that do not include fire
-- !^music	inverse-prefix-exact-match	Items that do not start with music
-- !.mp3$	inverse-suffix-exact-match	Items that do not end with .mp3

-- A single bar character term acts as an OR operator.
-- For example, the following query matches entries that start with core and end with either go, rb, or py.
-- ^core go$ | rb$ | py$


local status_ok, telescope = pcall(require, "telescope")
if not status_ok then
  vim.notify("telescope not found!")
  return
end

local actions = require "telescope.actions"

-- disable preview binaries
local previewers = require("telescope.previewers")
local Job = require("plenary.job")
local new_maker = function(filepath, bufnr, opts)
  filepath = vim.fn.expand(filepath)
  Job:new({
    command = "file",
    args = { "--mime-type", "-b", filepath },
    on_exit = function(j)
      local mime_type = vim.split(j:result()[1], "/")[1]
      if mime_type == "text" then
        previewers.buffer_previewer_maker(filepath, bufnr, opts)
      else
        -- maybe we want to write something to the buffer here
        vim.schedule(function()
          vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, { "BINARY" })
        end)
      end
    end
  }):sync()
end

telescope.setup {
  defaults = {
    buffer_previewer_maker = new_maker,

    prompt_prefix = " ",
    selection_caret = " ",
    path_display = {
      shorten= {
        -- e.g. for a path like
        --   `alpha/beta/gamma/delta.txt`
        -- setting `path_display.shorten = { len = 1, exclude = {1, -1} }`
        -- will give a path like:
        --   `alpha/b/g/delta.txt`
        len = 3, exclude = {1, -1}
      },
    },

    mappings = {
      i = {
        ["<C-n>"] = actions.cycle_history_next,
        ["<C-p>"] = actions.cycle_history_prev,

        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,

        ["<C-c>"] = actions.close,

        ["<Down>"] = actions.move_selection_next,
        ["<Up>"] = actions.move_selection_previous,

        ["<CR>"] = actions.select_default,
        ["<C-x>"] = actions.select_horizontal,
        ["<C-v>"] = actions.select_vertical,
        ["<C-t>"] = actions.select_tab,

        ["<C-u>"] = actions.preview_scrolling_up,
        ["<C-d>"] = actions.preview_scrolling_down,

        ["<PageUp>"] = actions.results_scrolling_up,
        ["<PageDown>"] = actions.results_scrolling_down,

        ["<Tab>"] = actions.toggle_selection + actions.move_selection_worse,
        ["<S-Tab>"] = actions.toggle_selection + actions.move_selection_better,
        ["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
        ["<M-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
        ["<C-l>"] = actions.complete_tag,
        ["<C-_>"] = actions.which_key, -- keys from pressing <C-/>
      },

      n = {
        ["<esc>"] = actions.close,
        ["<CR>"] = actions.select_default,
        ["<C-x>"] = actions.select_horizontal,
        ["<C-v>"] = actions.select_vertical,
        ["<C-t>"] = actions.select_tab,

        ["<Tab>"] = actions.toggle_selection + actions.move_selection_worse,
        ["<S-Tab>"] = actions.toggle_selection + actions.move_selection_better,
        ["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
        ["<M-q>"] = actions.send_selected_to_qflist + actions.open_qflist,

        ["j"] = actions.move_selection_next,
        ["k"] = actions.move_selection_previous,
        ["H"] = actions.move_to_top,
        ["M"] = actions.move_to_middle,
        ["L"] = actions.move_to_bottom,

        ["<Down>"] = actions.move_selection_next,
        ["<Up>"] = actions.move_selection_previous,
        ["gg"] = actions.move_to_top,
        ["G"] = actions.move_to_bottom,

        ["<C-u>"] = actions.preview_scrolling_up,
        ["<C-d>"] = actions.preview_scrolling_down,

        ["<PageUp>"] = actions.results_scrolling_up,
        ["<PageDown>"] = actions.results_scrolling_down,

        ["?"] = actions.which_key,
      },
    },
  },
  pickers = {
    find_files = {
      theme = "dropdown",
      previewer = false,
      -- find_command = { "find", "-type", "f" },
      find_command = {"fd"},
    },

    -- Default configuration for builtin pickers goes here:
    -- picker_name = {
    --   picker_config_key = value,
    --   ...
    -- }
    -- Now the picker_config_key will be applied every time you call this
    -- builtin picker
  },
  extensions = {
    -- Your extension configuration goes here:
    -- extension_name = {
    --   extension_config_key = value,
    -- }

    -- fzf syntax
    -- Token	Match type	Description
    -- sbtrkt	fuzzy-match	Items that match sbtrkt
    -- 'wild'	exact-match (quoted)	Items that include wild
    -- ^music	prefix-exact-match	Items that start with music
    -- .mp3$	suffix-exact-match	Items that end with .mp3
    -- !fire	inverse-exact-match	Items that do not include fire
    -- !^music	inverse-prefix-exact-match	Items that do not start with music
    -- !.mp3$	inverse-suffix-exact-match	Items that do not end with .mp3
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
      -- the default case_mode is "smart_case"
    },
    ["ui-select"] = {
      require("telescope.themes").get_dropdown {
        -- even more opts
      }
    },
  },
}

-- telescope.load_extension("frecency")
telescope.load_extension('fzf')
telescope.load_extension("ui-select")
telescope.load_extension('dap')
telescope.load_extension('vim_bookmarks')
-- load project extension. see project.lua file
