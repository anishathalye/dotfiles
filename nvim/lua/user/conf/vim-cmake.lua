vim.cmd(
  [[
    let g:cmake_root_markers = ['CMakeLists.txt', 'build', 'cmake-build'] 
    let g:cmake_link_compile_commands = 1
    let g:cmake_build_dir_location = './build'
    let g:cmake_build_options= ["-j8"]
]]
)
