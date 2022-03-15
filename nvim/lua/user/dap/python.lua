-- NOTE: make sure debugpy is installed
-- https://gitub.com/mfussenegger/nvim-dap/wiki/Debug-Adapter-installation#
local api = vim.api
local M = {}


M.test_runner = 'unittest'


local is_windows = function()
    return vim.loop.os_uname().sysname:find("Windows", 1, true) and true
end


local get_python_path = function()
  local venv_path = os.getenv('VIRTUAL_ENV')
  if venv_path then
    if is_windows() then
        return venv_path .. '\\Scripts\\python.exe'
    end
    return venv_path .. '/bin/python'
  end
  return nil
end


local enrich_config = function(config, on_config)
  if not config.pythonPath and not config.python then
    config.pythonPath = get_python_path()
  end
  on_config(config)
end


local default_setup_opts = {
  include_configs = true,
  console = 'integratedTerminal',
  pythonPath = nil,
}

local default_test_opts = {
  console = 'integratedTerminal'
}


local function load_dap()
  local ok, dap = pcall(require, 'dap')
  assert(ok, 'nvim-dap is required to use dap-python')
  return dap
end


--- Register the python debug adapter
function M.setup(adapter_python_path, opts)
  local dap = load_dap()
  adapter_python_path = vim.fn.expand(adapter_python_path)
  opts = vim.tbl_extend('keep', opts or {}, default_setup_opts)
  dap.adapters.python = function(cb, config)
    if config.request == 'attach' then
      cb({
        type = 'server';
        port = config.port or 0;
        host = config.host or '127.0.0.1';
        enrich_config = enrich_config;
      })
    else
      cb({
        type = 'executable';
        command = adapter_python_path;
        args = { '-m', 'debugpy.adapter' };
        enrich_config = enrich_config;
      })
    end
  end

  if opts.include_configs then
    dap.configurations.python = dap.configurations.python or {}
    table.insert(dap.configurations.python, {
      type = 'python';
      request = 'launch';
      name = 'Launch file';
      program = '${file}';
      console = opts.console;
      pythonPath = opts.pythonPath,
    })
    table.insert(dap.configurations.python, {
      type = 'python';
      request = 'launch';
      name = 'Launch file with arguments';
      program = '${file}';
      args = function()
        local args_string = vim.fn.input('Arguments: ')
        return vim.split(args_string, " +")
      end;
      console = opts.console;
      pythonPath = opts.pythonPath,
    })
    table.insert(dap.configurations.python, {
      type = 'python';
      request = 'attach';
      name = 'Attach remote';
      host = function()
        local value = vim.fn.input('Host [127.0.0.1]: ')
        if value ~= "" then
          return value
        end
        return '127.0.0.1'
      end;
      port = function()
        return tonumber(vim.fn.input('Port [5678]: ')) or 5678
      end;
    })
  end
end


local function get_nodes(query_text, predicate)
  local end_row = api.nvim_win_get_cursor(0)[1]
  local ft = api.nvim_buf_get_option(0, 'filetype')
  assert(ft == 'python', 'test_method of dap-python only works for python files, not ' .. ft)
  local query = vim.treesitter.parse_query(ft, query_text)
  assert(query, 'Could not parse treesitter query. Cannot find test')
  local parser = vim.treesitter.get_parser(0)
  local root = (parser:parse()[1]):root()
  local nodes = {}
  for _, node in query:iter_captures(root, 0, 0, end_row) do
    if predicate(node) then
      table.insert(nodes, node)
    end
  end
  return nodes
end


local function get_function_nodes()
  local query_text = [[
    (function_definition
      name: (identifier) @name) @definition.function
  ]]
  return get_nodes(query_text, function(node)
    return node:type() == 'identifier'
  end)
end


local function get_class_nodes()
  local query_text = [[
    (class_definition
       name: (identifier) @name) @definition.class
  ]]
  return get_nodes(query_text, function(node)
    return node:type() == 'identifier'
  end)
end


local function get_node_text(node)
  local row1, col1, row2, col2 = node:range()
  if row1 == row2 then
    row2 = row2 + 1
  end
  local lines = api.nvim_buf_get_lines(0, row1, row2, true)
  if #lines == 1 then
    return (lines[1]):sub(col1 + 1, col2)
  end
  return table.concat(lines, '\n')
end


local function get_parent_classname(node)
  local parent = node:parent()
  while parent do
    local type = parent:type()
    if type == 'class_definition' then
      for child in parent:iter_children() do
        if child:type() == 'identifier' then
          return get_node_text(child)
        end
      end
    end
    parent = parent:parent()
  end
end


local function prune_nil(items)
  return vim.tbl_filter(function(x) return x end, items)
end


local function trigger_test(classname, methodname, opts)
  local test_runner = opts.test_runner or M.test_runner
  local test_path
  local args
  if test_runner == 'unittest' then
    local path = vim.fn.expand('%:r:gs?/?.?')
    test_path = table.concat(prune_nil({path, classname, methodname}), '.')
    args = {'-v', test_path}
  elseif test_runner == 'pytest' then
    local path = vim.fn.expand('%:p')
    test_path = table.concat(prune_nil({path, classname, methodname}), '::')
    -- -s "allow output to stdout of test"
    args = {'-s', test_path}
  else
    print('Test runner `' .. test_runner .. '` not supported')
    return
  end
  load_dap().run({
    name = table.concat(prune_nil({classname, methodname}), '.'),
    type = 'python',
    request = 'launch',
    module = test_runner,
    args = args,
    console = opts.console
  })
end


local function closest_above_cursor(nodes)
  local result
  for _, node in pairs(nodes) do
    if not result then
      result = node
    else
      local node_row1, _, _, _ = node:range()
      local result_row1, _, _, _ = result:range()
      if node_row1 > result_row1 then
        result = node
      end
    end
  end
  return result
end


function M.test_class(opts)
  opts = vim.tbl_extend('keep', opts or {}, default_test_opts)
  local class_node = closest_above_cursor(get_class_nodes())
  if not class_node then
    print('No suitable test class found')
    return
  end
  local class = get_node_text(class_node)
  trigger_test(class, nil, opts)
end


function M.test_method(opts)
  opts = vim.tbl_extend('keep', opts or {}, default_test_opts)
  local function_node = closest_above_cursor(get_function_nodes())
  if not function_node then
    print('No suitable test method found')
    return
  end
  local class = get_parent_classname(function_node)
  local function_name = get_node_text(function_node)
  trigger_test(class, function_name, opts)
end


--- Strips extra whitespace at the start of the lines
--
-- >>> remove_indent({'    print(10)', '    if True:', '        print(20)'})
-- {'print(10)', 'if True:', '    print(20)'}
local function remove_indent(lines)
  local offset = nil
  for _, line in ipairs(lines) do
    local first_non_ws = line:find('[^%s]') or 0
    if first_non_ws >= 1 and (not offset or first_non_ws < offset) then
      offset = first_non_ws
    end
  end
  if offset > 1 then
    return vim.tbl_map(function(x) return string.sub(x, offset) end, lines)
  else
    return lines
  end
end


--- Debug the selected code
function M.debug_selection(opts)
  opts = vim.tbl_extend('keep', opts or {}, default_test_opts)
  local start_row, _ = unpack(api.nvim_buf_get_mark(0, '<'))
  local end_row, _ = unpack(api.nvim_buf_get_mark(0, '>'))
  local lines = api.nvim_buf_get_lines(0, start_row - 1, end_row, false)
  local code = table.concat(remove_indent(lines), '\n')
  load_dap().run({
    type = 'python',
    request = 'launch',
    code = code,
    console = opts.console
  })
end


return M
