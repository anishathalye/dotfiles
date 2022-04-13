-- \ 'cpp': 'std::cout << "{$}: " << {$} << std::endl;',
vim.cmd [[
let g:vim_printer_print_below_keybinding = '<leader>p'
let g:vim_printer_print_above_keybinding = '<leader>P'
let g:vim_printer_items  = { 
            \ 'python': 'print("{$}:", {$})', 
            \ 'javascript': 'console.log("{$}:", {$})',
            \ 'javascriptreact': 'console.log("{$}:", {$})',
            \ 'javascript.jsx': 'console.log("{$}:", {$})',
            \ 'typescript': 'console.log("{$}:", {$})',
            \ 'typescript.tsx': 'console.log("{$}:", {$})',
            \ 'go': 'fmt.Printf("{$}:%+v\n", {$})',
            \ 'vim': 'echo "{$}: ".{$}',
            \ 'rust': 'println!("{$}: {:?}", {$});',
            \ 'sh': 'echo "{$}: ${$}"',
            \ 'bash': 'echo "{$}: ${$}"',
            \ 'zsh': 'echo "{$}: ${$}"',
            \ 'java': 'System.out.println("{$}: " + {$});',
            \ 'lua': 'print("{$}: " .. {$})',
            \ 'c': 'printf("{$}:%d\n", {$});',
            \ 'cpp': 'printf("{$}:%d\n", {$});',
            \ 'f77': 'write(6,*)"{$}: ", {$}',
            \ 'f90': 'write(6,*)"{$}: ", {$}' 
            \ }
]]
