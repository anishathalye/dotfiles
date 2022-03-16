vim.cmd(
  [[
    " build project from cwd
    let g:cmake_root_markers = ['CMakeLists.txt'] 
    let g:cmake_link_compile_commands = 1
    let g:cmake_build_dir_location = './build'
    let g:cmake_build_options= ["-j8"]
]]
)
