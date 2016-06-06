nnoremap <buffer> <Left> :GotoDot<CR>
nnoremap <buffer> <Right> :CoqToCursor<CR>
nnoremap <buffer> <Up> :CoqUndo<CR>
nnoremap <buffer> <Down> :CoqNext<CR>

inoremap <buffer> <Left> <C-o>:GotoDot<CR>
inoremap <buffer> <Right> <C-o>:CoqToCursor<CR>
inoremap <buffer> <Up> <C-o>:CoqUndo<CR>
inoremap <buffer> <Down> <C-o>:CoqNext<CR>

" this is hacky, but it works
let b:coq_running = 0
function! CoqToggle()
    if b:coq_running == 1
        execute "CoqKill"
        let b:coq_running = 0
    else
        execute "CoqLaunch"
        let b:coq_running = 1
    endif
endfunction
nnoremap <buffer> <Leader>c :call CoqToggle()<CR>
