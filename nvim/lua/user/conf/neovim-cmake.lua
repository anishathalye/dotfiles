-- CMake followed by
-- Argument	Description
-- configure ...	Configure project. It uses g:cmake_build_dir as a build folder. It will also generate compile_commands.json and symlink it to the project directory. Additional arguments will be passed to CMake. Example: CMake configure -G Ninja.
-- build ...	Compile selected target (via --build). Additional arguments will be passed to CMake.
-- build_all ...	Same as above, but will build all rule.
-- run ...	Run selected target. Additional arguments will be passed to the target being launched.
-- debug ...	Run debugging on selected target. Additional arguments will be passed to the target being launched.
-- clean ...	Execute clear target. Additional arguments will be passed to CMake.
-- build_and_run ...	Execute CMake build and, if build successful, then CMake run. Additional arguments will be passed to CMake.
-- build_and_debug ...	Execute CMake build and, if build successful, then CMake debug. Additional arguments will be passed to CMake.
-- set_target_args	Set arguments for running / debugging target.
-- clear_cache	Remove CMakeCache.txt file from the build directory.
-- open_build_dir	Open current build folder via xdg-open (Linux) or start (Windows).
-- select_build_type	Select build type (Release, Debug, etc.).
-- select_target	Select target for running / debugging.
-- create_project	Create new CMake project.
-- cancel	Cancel current running CMake action like build or run.

local Path = require('plenary.path')
local status_ok, cmake = pcall(require, "cmake")
if not status_ok then
  vim.notify("cmake not found!")
  return
end

cmake.setup({
  cmake_executable = 'cmake', -- CMake executable to run.
  parameters_file = 'neovim.json', -- JSON file to store information about selected target, run arguments and build type.
  build_dir = tostring(Path:new('{cwd}', 'build', '{os}-{build_type}')), -- Build directory. The expressions `{cwd}`, `{os}` and `{build_type}` will be expanded with the corresponding text values.
  -- samples_path = tostring(script_path:parent():parent():parent() / 'samples'), -- Folder with samples. `samples` folder from the plugin directory is used by default.
  default_projects_path = tostring(Path:new(vim.loop.os_homedir(), 'Projects')), -- Default folder for creating project.
  configure_args = { '-D', 'CMAKE_EXPORT_COMPILE_COMMANDS=1' }, -- Default arguments that will be always passed at cmake configure step. By default tells cmake to generate `compile_commands.json`.
  build_args = {
    '-j8',
  }, -- Default arguments that will be always passed at cmake build step.
  on_build_output = nil, -- Callback which will be called on every line that is printed during build process. Accepts printed line as argument.
  quickfix_height = 10, -- Height of the opened quickfix.
  quickfix_only_on_error = false, -- Open quickfix window only if target build failed.
  dap_configuration = {
    name = "Launch file",
    type = "cppdbg",
    request = "launch",
    cwd = '${workspaceFolder}',
    stopOnEntry = false,
    runInTerminal = true,
    setupCommands = {
      {
        description = 'enable pretty printing',
        text = '-enable-pretty-printing',
        ignoreFailures = false
      },
    },
  }, -- DAP configuration. By default configured to work with `lldb-vscode`.
  dap_open_command = require('dap').repl.open, -- Command to run after starting DAP session. You can set it to `false` if you don't want to open anything or `require('dapui').open` if you are using https://github.com/rcarriga/nvim-dap-ui
})
