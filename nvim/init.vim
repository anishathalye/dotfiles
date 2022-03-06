" packadd quickscope

" execute 'luafile ' . stdpath('config') . '/lua/settings.lua'

function! s:manageEditorSize(...)
    let count = a:1
    let to = a:2
    for i in range(1, count ? count : 1)
        call VSCodeNotify(to == 'increase' ? 'workbench.action.increaseViewSize' : 'workbench.action.decreaseViewSize')
    endfor
endfunction

function! s:vscodeCommentary(...) abort
    if !a:0
        let &operatorfunc = matchstr(expand('<sfile>'), '[^. ]*$')
        return 'g@'
    elseif a:0 > 1
        let [line1, line2] = [a:1, a:2]
    else
        let [line1, line2] = [line("'["), line("']")]
    endif

    call VSCodeCallRange("editor.action.commentLine", line1, line2, 0)
endfunction

function! s:openVSCodeCommandsInVisualMode()
    normal! gv
    let visualmode = visualmode()
    if visualmode == "V"
        let startLine = line("v")
        let endLine = line(".")
        call VSCodeNotifyRange("workbench.action.showCommands", startLine, endLine, 1)
    else
        let startPos = getpos("v")
        let endPos = getpos(".")
        call VSCodeNotifyRangePos("workbench.action.showCommands", startPos[1], endPos[1], startPos[2], endPos[2], 1)
    endif
endfunction

function! s:openWhichKeyInVisualMode()
    normal! gv
    let visualmode = visualmode()
    if visualmode == "V"
        let startLine = line("v")
        let endLine = line(".")
        call VSCodeNotifyRange("whichkey.show", startLine, endLine, 1)
    else
        let startPos = getpos("v")
        let endPos = getpos(".")
        call VSCodeNotifyRangePos("whichkey.show", startPos[1], endPos[1], startPos[2], endPos[2], 1)
    endif
endfunction

" Better Navigation
nnoremap <silent> <C-j> :call VSCodeNotify('workbench.action.navigateDown')<CR>
xnoremap <silent> <C-j> :call VSCodeNotify('workbench.action.navigateDown')<CR>
nnoremap <silent> <C-k> :call VSCodeNotify('workbench.action.navigateUp')<CR>
xnoremap <silent> <C-k> :call VSCodeNotify('workbench.action.navigateUp')<CR>
nnoremap <silent> <C-h> :call VSCodeNotify('workbench.action.navigateLeft')<CR>
xnoremap <silent> <C-h> :call VSCodeNotify('workbench.action.navigateLeft')<CR>
nnoremap <silent> <C-l> :call VSCodeNotify('workbench.action.navigateRight')<CR>
xnoremap <silent> <C-l> :call VSCodeNotify('workbench.action.navigateRight')<CR>


" Bind C-/ to vscode commentary since calling from vscode produces double comments due to multiple cursors
xnoremap <expr> <C-/> <SID>vscodeCommentary()
nnoremap <expr> <C-/> <SID>vscodeCommentary() . '_'

nnoremap <silent> <C-w>_ :<C-u>call VSCodeNotify('workbench.action.toggleEditorWidths')<CR>

nnoremap <silent> <Space> :call VSCodeNotify('whichkey.show')<CR>
xnoremap <silent> <Space> :<C-u>call <SID>openWhichKeyInVisualMode()<CR>

xnoremap <silent> <C-P> :<C-u>call <SID>openVSCodeCommandsInVisualMode()<CR>

xmap gc  <Plug>VSCodeCommentary
nmap gc  <Plug>VSCodeCommentary
omap gc  <Plug>VSCodeCommentary
nmap gcc <Plug>VSCodeCommentaryLine

" Simulate same TAB behavior in VSCode
nmap <Tab> :Tabnext<CR>
nmap <S-Tab> :Tabprev<CR>

" Custom config
" 共享系统粘贴板
set clipboard+=unnamed
" 设置前导键
let mapleader=";"
" 暂时取消搜索高亮快捷键
nnoremap <silent> <Leader>l :<C-u>nohlsearch<CR><C-l>

" 移动相关
" 前一个缓冲区
nnoremap <silent> [b :w<CR>:bprevious<CR>
" 后一个缓冲区
nnoremap <silent> ]b :w<CR>:bnext<CR>
" 定义快捷键到行首和行尾
map H ^
map L $
" 定义快速跳转
nmap <Leader>t <C-]>
" 定义快速跳转回退
nmap <Leader>T <C-t>
" 标签页后退 ---标签页前进是gt
nmap R gt
nmap E gT

" 文件操作相关
" 定义快捷键关闭当前分割窗口
nmap <Leader>q :q<CR>
" 定义快捷键保存当前窗口内容
nmap <Leader>w :w<CR>

" 窗口操作相关
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Focus center or current cursor position after \"J\"
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap J mzJ`z
nnoremap j jzz
nnoremap k kzz

" Moving text
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Fold and Un fold
nnoremap <silent> zc :call VSCodeNotify('editor.fold')<CR>
nnoremap <silent> zo :call VSCodeNotify('editor.unfold')<CR>
nnoremap <silent> zm :call VSCodeNotify('editor.foldAll')<CR>
nnoremap <silent> zR :call VSCodeNotify('editor.unfoldAll')<CR>

" References
nnoremap gr :call VSCodeNotify('editor.action.goToReferences')<CR>
nnoremap <leader>u :call VSCodeNotify('editor.action.goToReferences')<CR>
nnoremap <leader>U :call VSCodeNotify('references-view.find')<CR>



"==========================
"	Plugin
"==========================
call plug#begin()
" Plug 'asvetliakov/vim-easymotion', { 'as': 'vsc-easymotion' }
call plug#end()


