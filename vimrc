set nocompatible
set encoding=utf-8

set number
set linebreak
set showbreak=+++
set textwidth=80
set showmatch
set showcmd
set cursorline

set visualbell

set hlsearch
set smartcase
set ignorecase
set incsearch

set autoindent
set cindent
set shiftwidth=4
set smartindent
set smarttab
set softtabstop=4 noexpandtab
set wrap

set ruler
set undolevels=1000
set backspace=indent,eol,start

set spell spelllang=cs,en
set mouse=a

call plug#begin('~/.vim/plugged')
    Plug 'vim-airline/vim-airline'
    Plug 'tomasiser/vim-code-dark'
    Plug 'w0rp/ale'
    Plug 'romainl/vim-cool'
call plug#end()

colorscheme codedark
let g:airline_theme = 'codedark'
if &term =~ '256color'
    set t_ut=
endif

" powerline
let g:airline_powerline_fonts = 1
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" ALE
let g:ale_completion_enabled = 1
let g:ale_linters = { 'c': ['gcc', 'clangtidy', 'clang-format'], 'cpp': ['clang', 'clangtidy', 'clang-format'],    'haskell': ['ghc', 'hlint'] }

" c
let g:ale_c_gcc_executable = 'gcc'
let g:ale_c_gcc_options = '-std=c99 -Wall -Wextra -pedantic'
let g:ale_c_clangtidy_executable = 'clang-tidy'
let g:ale_c_clangtidy_options = '-std=c99 -Wall -Wextra -pedantic'

" cpp
let g:ale_cpp_clang_executable = 'clang++'
let g:ale_cpp_clang_options = '-std=c++17 -Wall -Wextra -pedantic -Wall-style-cast'
let g:ale_cpp_clangtidy_executable = 'clang-tidy'
let g:ale_cpp_clangtidy_options = '-std=c++17 -Wall -Wextra -pedantic -Wold-style-cast'

" haskell
let g:ale_haskell_ghc_options = '-dynamic -fno-code -v0'
