" Plugin manager:
if empty(glob('~/.vim/autoload/plug.vim'))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Plugins:
call plug#begin('~/.vim/plugged')
Plug 'junegunn/vim-easy-align'
Plug 'dracula/vim',{'name':'dracula'}
Plug 'sheerun/vim-polyglot'
call plug#end()


" Colortheme:
let g:dracula_italic = 0
colorscheme dracula
syntax enable
set t_Co=16

" General:
set number
set linebreak
set showbreak=+++
set showmatch

set hlsearch
set smartcase
set ignorecase
set incsearch

set autoindent
set cindent
set smartindent
set expandtab
set smarttab
set shiftwidth=4
set softtabstop=4

set ruler
set undolevels=1000
set backspace=indent,eol,start
