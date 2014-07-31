" Vim plugin that add the entries in a .gitignore file to 'wildignore'

if exists("g:loaded_gitignore_wildignore")
  finish
endif
let g:loaded_gitignore_wildignore = 1

let s:save_cpo = &cpo
set cpo&vim

function s:WildignoreFromGitignore(...)
  let gitignore = (a:0 && !empty(a:1)) ? fnamemodify(a:1, ':p') : fnamemodify(expand('%'), ':p:h') . '/'
  let gitignore .= '.gitignore'
  if filereadable(gitignore)
    let igstring = ''
    for oline in readfile(gitignore)
      let line = substitute(oline, '\s|\n|\r', '', "g")
      if line =~ '^#' | con | endif
      if line == ''   | con | endif
      if line =~ '^!' | con | endif
      if line =~ '/$' | let igstring .= "," . "*/" . line . "*" | con | endif
      let igstring .= "," . line
    endfor
    let execstring = "set wildignore+=".substitute(igstring, '^,', '', "g")
    execute execstring
  endif
endfunction

noremap <unique> <script> <Plug>WildignoreFromGitignore <SID>WildignoreFromGitignore
noremap <SID>WildignoreFromGitignore :call <SID>WildignoreFromGitignore()<CR>

command -nargs=? WildignoreFromGitignore :call <SID>WildignoreFromGitignore(<q-args>)

augroup wildignorefromgitignore_fugitive
  autocmd!
  autocmd User Fugitive if exists('b:git_dir') | call <SID>WildignoreFromGitignore(fnamemodify(b:git_dir, ':h')) | endif
augroup END

let &cpo = s:save_cpo

" vim:set ft=vim sw=2 sts=2 et :
