set nocompatible " not vi compatible

"------------------
" Syntax and indent
"------------------
syntax on " turn on syntax highlighting
set showmatch " show matching braces when text indicator is over them
set cursorline " highlight current line

colorscheme grb256

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
set backspace=indent,eol,start " allow backspacing over everything
set timeout timeoutlen=1000 ttimeoutlen=100 " fix slow O inserts
" use 2 spaces instead of tabs during formatting
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
" smart case-sensitive search
set ignorecase
set smartcase
" tab completion for files/bufferss
set wildmode=longest,list
set wildmenu

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
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>
