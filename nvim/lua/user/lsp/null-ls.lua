local null_ls_status_ok, null_ls = pcall(require, "null-ls")
if not null_ls_status_ok then
  vim.notify("null-ls not found!")
	return
end

-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
local formatting = null_ls.builtins.formatting
-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
local diagnostics = null_ls.builtins.diagnostics

local completion = null_ls.builtins.completion

null_ls.setup({
	debug = false,
	sources = {
		formatting.autopep8, -- for python
		formatting.stylua,     -- for lua
    formatting.clang_format, -- for cpp
    formatting.gofmt,       -- for golang

    -- diagnostics.flake8,

    completion.spell,
	},
  -- you can reuse a shared lspconfig on_attach callback here
  on_attach = function(client)
      -- NOTE: 如果想要禁止某种语言在save时format，可以添加判定
      -- if client.name == "xxx" then
      --
      -- end
      -- auto format when save file
    if client.resolved_capabilities.document_formatting then
      vim.cmd([[
            augroup LspFormatting
            autocmd! * <buffer>
            autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()
            augroup END
            ]])
    end
  end,
})
