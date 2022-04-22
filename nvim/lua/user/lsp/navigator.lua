-- 本插件配置并未使用，仅做保存
local path = require 'nvim-lsp-installer.path'
local install_root_dir = path.concat { vim.fn.stdpath 'data', 'lsp_servers' }

local lsp_handlers = require 'user.lsp.handlers'

require 'navigator'.setup({
  debug = true, -- log output, set to true and log path: ~/.cache/nvim/gh.log
  width = 0.75, -- max width ratio (number of cols for the floating window) / (window width)
  height = 0.3, -- max list window height, 0.3 by default
  preview_height = 0.35, -- max height of preview windows
  icons = {
    -- Code action
    code_action_icon = "🏏",
    -- Diagnostics
    diagnostic_head = '🐛',
    diagnostic_head_severity_1 = "🈲",
    -- refer to lua/navigator.lua for more icons setups
  },

  default_mapping = true, -- set to false if you will remap every key
  -- keymaps = { { key = "gK", func = "declaration()" } }, -- a list of key maps
  -- this kepmap gK will override "gD" mapping function declaration()  in default kepmap
  -- please check mapping.lua for all keymaps
  treesitter_analysis = true, -- treesitter variable context
  transparency = nil, -- 0 ~ 100 blur the main window, 100: fully transparent, 0: opaque,  set to nil or 100 to disable it

  on_attach = lsp_handlers.on_attach,
  lsp_signature_help = false, -- if you would like to hook ray-x/lsp_signature plugin in navigator
  -- setup here. if it is nil, navigator will not init signature help
  signature_help_cfg = nil, -- if you would like to init ray-x/lsp_signature plugin in navigator, and pass in your own config to signature help
  lsp_installer = false, -- set to true if you would like use the lsp installed by williamboman/nvim-lsp-installer
  lsp = {
    code_action = { enable = true, sign = true, sign_priority = 40, virtual_text = true },
    code_lens_action = { enable = true, sign = true, sign_priority = 40, virtual_text = true },
    format_on_save = true, -- set to false to disable lsp code format on save (if you are using prettier/efm/formater etc)
    disable_format_cap = { "sqls", "sumneko_lua" }, -- a list of lsp disable format capacity (e.g. if you using efm or vim-codeformat etc), empty {} by default
    disable_lsp = { 'ccls', }, -- a list of lsp server disabled for your project, e.g. denols and tsserver you may
    -- only want to enable one lsp server
    -- to disable all default config and use your own lsp setup set
    -- disable_lsp = 'all'
    -- Default {}
    diagnostic = {
      underline = true,
      virtual_text = false, -- show virtual for diagnostic message
      update_in_insert = false, -- update diagnostic message in insert mode
    },
    diagnostic_scrollbar_sign = false, -- experimental:  diagnostic status in scroll bar area; set to false to disable the diagnostic sign,
    -- for other style, set to {'╍', 'ﮆ'} or {'-', '='}

    -- every lsp svrs config
    gopls = { -- gopls setting
      cmd = { install_root_dir .. '/go/gopls' },
      settings = {
        gopls = { gofumpt = false } -- disable gofumpt etc,
      },
    },

    clangd = { -- clangd settings
      cmd = { install_root_dir .. '/clangd/clangd/bin/clangd' },
    },

    -- servers = { 'cmake', 'ltex' }, -- by default empty, and it should load all LSP clients avalible based on filetype
    -- but if you want navigator load  e.g. `cmake` and `ltex` for you , you
    -- can put them in the `servers` list and navigator will auto load them.
    -- you could still specify the custom config  like this
    -- cmake = {filetypes = {'cmake', 'makefile'}, single_file_support = false},
  }
}
)
