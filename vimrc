set nocompatible
set encoding=utf-8

set number
set linebreak
set showbreak=+++
set showmatch
set showcmd
set cursorline

set hlsearch
set smartcase
set ignorecase
set incsearch

set autoindent
set cindent
set shiftwidth=4
set smarttab
set tabstop=4
set expandtab
set wrap

set ruler
set undolevels=1000
set backspace=indent,eol,start

set spell spelllang=cs,en
set mouse=a

filetype plugin on

"""""""""""""""""""
""" Key mapping """
"""""""""""""""""""

nnoremap <C-j> :m .+1<CR>==
nnoremap <C-k> :m .-2<CR>==
inoremap <C-j> <Esc>:m .+1<CR>==gi
inoremap <C-k> <Esc>:m .-2<CR>==gi
vnoremap <C-j> :m '>+1<CR>gv=gv
vnoremap <C-k> :m '<-2<CR>gv=gv

" Disable search highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>


"""""""""""
""" ALE """
"""""""""""
let g:ale_completion_enabled = 1

let g:ale_linters = {
\   'c': ['clangd', 'clang-format'],
\   'cpp': ['clangd', 'clang-format'],
\   'haskell': ['ghc', 'hlint']
\}

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'c': ['clang-format'],
\   'cpp': ['clang-format'],
\   'haskell': ['hlint']
\}

" c
let g:ale_c_gcc_options = '-std=c11 -Wall -Wextra -pedantic'

" cpp
let g:ale_cpp_clang_options = '-std=c++17 -Wall -Wextra -pedantic -Wold-style-cast'
let g:ale_cpp_clangd_options = '-compile-commands-dir=~/redhat/divine/divine-current-release/'

" haskell
let g:ale_haskell_ghc_options = '-dynamic -fno-code -v0'

" nerdtree
map <C-n> :NERDTreeToggle<CR>

" tagbar
map <F8> :TagbarToggle<CR>

" airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#ale#enabled = 1
let g:airline_skip_empty_sections = 1

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

let g:airline_symbols.linenr = '␊'
let g:airline_symbols.whitespace = 'Ξ'

call plug#begin('~/.vim/plugged')
    Plug 'vim-airline/vim-airline'
    Plug 'tomasiser/vim-code-dark'
    Plug 'dense-analysis/ale'
    Plug 'romainl/vim-cool'
    Plug 'tpope/vim-fugitive'
    Plug 'scrooloose/nerdtree'
    Plug 'scrooloose/nerdcommenter'
    Plug 'Xuyuanp/nerdtree-git-plugin'
    Plug 'ryanoasis/vim-devicons'
    Plug 'majutsushi/tagbar'
    Plug 'tpope/vim-surround'
    Plug 'airblade/vim-gitgutter'
    Plug 'junegunn/fzf.vim'
    Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
call plug#end()

colorscheme codedark
let g:airline_theme = 'codedark'
if &term =~ '256color'
    set t_ut=
endif

function! s:RotateString(string)
    let split_string = split(a:string, '\zs')
    return join(split_string[-1:] + split_string[:-2], '')
endfunction

function! s:RotateLine(line)
    return substitute(
        \ a:line,
        \ '^\(\s*\)\(.\{-}\)\(\s*\)$',
        \ '\=submatch(1) . <SID>RotateString(submatch(2)) . submatch(3)',
        \ ''
    \ )
endfunction

function! s:RotateLines()
    let saved_view = winsaveview()
    let first_visible_line = line('w0')
    let last_visible_line = line('w$')
    let lines = getline(first_visible_line, last_visible_line)
    try
        while 1 " <C-c> to exit
            call map(lines, '<SID>RotateLine(v:val)')
            call setline(first_visible_line, lines)
            redraw
            sleep 50m
        endwhile
    finally
        if &modified
            silent undo
        endif
        call winrestview(saved_view)
    endtry
endfunction

nnoremap <silent> <Plug>(RotateLines) :<C-u>call <SID>RotateLines()<CR>

nmap \r <Plug>(RotateLines)
