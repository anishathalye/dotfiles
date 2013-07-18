" Vim plugin that add the entries in a .gitignore file to 'wildignore'
" Last Change:	2012 Aug 6
" Maintainer:	Adam Bellaire
" Contributors:	Giuseppe Rota
" License:	This file is placed in the public domain.
"
"                     Fugitive Integration
" The plugin provides no default mappings but integrates nicely with fugitive
" https://github.com/tpope/vim-fugitive. I.e. if you have fugitive installed,
" this plugin will use fugitive's builtin detection of a git repository and
" add that repo's gitignore entries to 'wildignore'
"
" If you don't want that to happen automatically, create the file
" `.vim/after/plugin/disable-gitignore-fugitive.vim` with the single command:
" autocmd! wildignorefromgitignore_fugitive
"
"                     Manual Triggering
" If you need to invoke the functionality manually, put this in your .vimrc:
" map <silent> <unique> <Leader>foo <Plug>WildignoreFromGitignore
" which will look for a .gitignore file in the same directory as the current
" file.
"
" You can also map against the :WildignoreFromGitignore command that accepts
" a directory name as in:
" map <Leader>baz :WildignoreFromGitignore /path/to/some/repo<CR>

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
      " if line =~ '/$' | let igstring .= "," . line . "*" | con | endif
      if line =~ '/$' | let igstring .= "," . "*/" . line . "*" | con | endif " fix directory matching
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

" vim:set ft=vim sw=2 sts=2 et:
