#!/bin/bash
echo "Configuring tpope's pathogen"

echo "creating Directories"
rm -rf ~/.vim/autoload
mkdir -p ~/.vim/autoload

rm -rf ~/.vim/bundle
mkdir -p ~/.vim/bundle

echo downloading
curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

echo "getting vim config"
rm -rf ~/.vimrc
cd ~/
wget https://raw.githubusercontent.com/kstatz12/dotfiles/master/.vimrc

echo "installing plugins"
cd ~/.vim/bundle


git clone https://github.com/scrooloose/nerdtree

git clone https://github.com/scrooloose/nerdcommenter

git clone https://github.com/bling/vim-airline

git clone https://github.com/slashmili/alchemist.vim


