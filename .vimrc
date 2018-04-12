execute pathogen#infect()

set nobackup
syntax on
filetype plugin indent on
:set number

set clipboard=unnamedplus


autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

map <f2> :NERDTreeToggle<CR>

let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

let mapleader=","
set timeout timeoutlen=1500

