set nocompatible " not vi compatible

"--------------
" Load pathogen
"--------------
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

"------------------
" Syntax and indent
"------------------
syntax on " turn on syntax highlighting
set showmatch " show matching braces when text indicator is over them
set cursorline " highlight current line

" vim can autodetect this based on $TERM (e.g. 'xterm-256color')
" but it can be set to force 256 colors
" set t_Co=256
" colorscheme womprat
if &t_Co < 256
    colorscheme default
    set nocursorline " looks bad in this mode
else
    set background=dark
    let g:solarized_termcolors=256 " instead of 16 color with mapping in terminal
    colorscheme solarized
    " customized colors
    highlight SignColumn ctermbg=234
    highlight StatusLine cterm=bold ctermfg=245 ctermbg=none
    highlight StatusLineNC cterm=bold ctermfg=245 ctermbg=none
    let g:NeatStatusLine_color_normal='ctermfg=64 ctermbg=0 cterm=bold'
    let g:NeatStatusLine_color_insert='ctermfg=136 ctermbg=0 cterm=bold'
    let g:NeatStatusLine_color_replace='ctermfg=160 ctermbg=0 cterm=bold'
    let g:NeatStatusLine_color_visual='ctermfg=33 ctermbg=0 cterm=bold'
    let g:NeatStatusLine_color_position='ctermfg=245 ctermbg=0 cterm=bold'
    let g:NeatStatusLine_color_modified='ctermfg=166 ctermbg=0 cterm=bold'
    let g:NeatStatusLine_color_line='ctermfg=61 ctermbg=0 cterm=bold'
    let g:NeatStatusLine_color_filetype='ctermfg=37 ctermbg=0 cterm=bold'
endif

filetype plugin indent on " enable file type detection
set autoindent

"---------------------
" Basic editing config
"---------------------
set nu " number lines
set incsearch " incremental search (as string is being typed)
set hls " highlight search
set lbr " line break
set ruler " show current position in file
set noshowmode " hide mode
set backspace=indent,eol,start " allow backspacing over everything
set timeout timeoutlen=1000 ttimeoutlen=100 " fix slow O inserts
set autochdir " automatically set current directory to directory of last opened file
set hidden " allow auto-hiding of edited buffers
set history=4096 " more history
" use 2 spaces instead of tabs during formatting
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
" smart case-sensitive search
set ignorecase
set smartcase
" tab completion for files/bufferss
set wildmode=longest,list
set wildmenu
set mouse+=a " enable mouse mode (scrolling, selection, etc)
if &term =~ '^screen'
    " tmux knows the extended mouse mode
    set ttymouse=xterm2
endif

"-------------
" Disable keys
"-------------
cabbrev X echoe "You probably don't want to encrypt this file"<CR>

"---------------------
" Multipurpose tab key
"---------------------
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>

"-------------------
" Disable arrow keys
"-------------------
nnoremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
nnoremap <Down> :echoe "Use j"<CR>

"--------------------
" Misc configurations
"--------------------

" open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" tagbar
nnoremap <C-\> :TagbarToggle<CR>

"---------------------
" Local customizations
"---------------------

" local customizations in ~/.vimrc_local
let $LOCALFILE=expand("~/.vimrc_local")
if filereadable($LOCALFILE)
    source $LOCALFILE
endif
