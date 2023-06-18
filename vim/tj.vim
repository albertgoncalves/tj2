" $ cp tj.vim ~/.vim/syntax/tj.vim
" $ grep '.tj' ~/.vimrc
" autocmd BufNewFile,BufRead *.tj setlocal filetype=tj

if exists("b:current_syntax")
    finish
endif

syn match Delimiter "\([(),:=\\{}]\)\|\(\->\)"

syn match Type "\'\<[a-z]\+\>"

syn match Constant "\<[A-Z][A-Za-z0-9]*\>"

syn keyword Todo FIXME NOTE TODO contained

syn match Comment "#.*" contains=Todo

let b:current_syntax = "tj"
