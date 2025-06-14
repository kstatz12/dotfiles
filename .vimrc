" Basic settings
set nocompatible              " Use Vim defaults (not Vi)
filetype off                  " Required for plugin loading

" Encoding
set encoding=utf-8

" Line numbers
set number

" Tabs & indentation
set tabstop=4                 " A tab is 4 spaces
set shiftwidth=4              " Indents are 4 spaces
set expandtab                 " Convert tabs to spaces
set autoindent
set smartindent

" Searching
set ignorecase
set smartcase
set incsearch
set hlsearch

" UI enhancements
syntax on
set showcmd
set cursorline
set wildmenu
set lazyredraw
set showmatch
set background=dark

" Clipboard
set clipboard=unnamedplus     " Use system clipboard

" File handling
set hidden                    " Allow buffer switching without saving
set nobackup
set nowritebackup
set noswapfile

" Split window behavior
set splitright
set splitbelow

" Enable mouse
set mouse=a

" Set leader key
let mapleader = ","

nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>x :x<CR>
nnoremap <leader>h :nohlsearch<CR>
nnoremap <C-p> :Files<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>r :Rg<CR>
nnoremap <C-n> :NERDTreeToggle<CR>
nnoremap <Leader>wv :vsplit<CR>
nnoremap <Leader>wl :split<CR>

nnoremap <Leader>wh <C-w>h
nnoremap <Leader>wj <C-w>j
nnoremap <Leader>wk <C-w>k
nnoremap <Leader>wl <C-w>l  " Note: same as split — choose only one or rename one

nnoremap <Leader>w<Left>  :vertical resize -4<CR>
nnoremap <Leader>w<Right> :vertical resize +4<CR>
nnoremap <Leader>w<Up>    :resize +2<CR>
nnoremap <Leader>w<Down>  :resize -2<CR>

" Close current window
nnoremap <Leader>wc :close<CR>

" Keep only the current window
nnoremap <Leader>wo :only<CR>

" Enable filetype plugins and indentation
filetype plugin indent on


" Load vim-plug plugin manager
call plug#begin('~/.vim/plugged')

" Plugins
Plug 'preservim/nerdtree'        " File explorer
Plug 'vim-airline/vim-airline'   " Status/tabline
Plug 'neoclide/coc.nvim', {'branch': 'release'} " Intellisense engine
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'mhinz/vim-startify'

call plug#end()
