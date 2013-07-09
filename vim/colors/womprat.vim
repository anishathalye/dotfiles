runtime colors/wombat256mod.vim

let colors_name = "womprat"

" Color names
" http://vim.wikia.com/wiki/Xterm256_color_names_for_console_Vim

" Remove background colors
hi Normal		ctermfg=252		ctermbg=none		cterm=none		guifg=#e3e0d7	guibg=none	gui=none
hi LineNr		ctermfg=241		ctermbg=none		cterm=none		guifg=#857b6f	guibg=none	gui=none

" Change search highlight colors to Grey19 on Yellow3
hi Search		ctermfg=236		ctermbg=184		cterm=none		guifg=#303030	guibg=#d7d700	gui=none

" Change the syntax highlighting colors around
"hi Keyword		ctermfg=111		cterm=none		guifg=#88b8f6	gui=none
"hi Statement	ctermfg=111		cterm=none		guifg=#88b8f6	gui=none
"hi Constant		ctermfg=173		cterm=none		guifg=#e5786d	gui=none
"hi Number		ctermfg=173		cterm=none		guifg=#e5786d	gui=none
"hi PreProc		ctermfg=173		cterm=none		guifg=#e5786d	gui=none
"hi Function		ctermfg=192		cterm=none		guifg=#cae982	gui=none
"hi Identifier	ctermfg=192		cterm=none		guifg=#cae982	gui=none
"hi Type			ctermfg=186		cterm=none		guifg=#d4d987	gui=none
"hi Special		ctermfg=229		cterm=none		guifg=#eadead	gui=none
"hi String		ctermfg=113		cterm=none		guifg=#95e454	gui=italic
"hi Comment		ctermfg=246		cterm=none		guifg=#9c998e	gui=italic
"hi Todo			ctermfg=101		cterm=none		guifg=#857b6f	gui=italic

" Change keywords and statements to SkyBlue3
hi Keyword		ctermfg=74		cterm=none		guifg=#5fafd7	gui=none
hi Statement	ctermfg=74		cterm=none		guifg=#5fafd7	gui=none

" Change functions and identifiers to DarkOliveGreen3
hi Function		ctermfg=113		cterm=none		guifg=#87d75f	gui=none
hi Identifier	ctermfg=113		cterm=none		guifg=#87d75f	gui=none

" Change strings to IndianRed
hi String		ctermfg=131		cterm=none		guifg=#af5f5f	gui=italic

" Switch some of the diff colors around so red and green are removed and added
hi def link diffRemoved         String
