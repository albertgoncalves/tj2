" $ cp tj.vim ~/.vim/syntax/tj.vim
" $ grep '.tj' ~/.vimrc
" autocmd BufNewFile,BufRead *.tj setlocal filetype=tj

if exists("b:current_syntax")
    finish
endif

syn match Operator "[(),\-:=>\\{}]"

syn match Comment "#.*"

let b:current_syntax = "tj"
