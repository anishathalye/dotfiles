#!/bin/bash
# Script to link all the files in dotfiles to the home directory

BASEDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# note: when linking directories,
# they must be linked like `ln -s ${BASEDIR}/dir/ ~/.dir`
# with a trailing slash in front of the original directory name

# tmux
ln -s ${BASEDIR}/tmux.conf ~/.tmux.conf

# screen
ln -s ${BASEDIR}/screenrc ~/.screenrc

# vim
ln -s ${BASEDIR}/vimrc ~/.vimrc
ln -s ${BASEDIR}/vim/ ~/.vim

# zsh
ln -s ${BASEDIR}/zshrc ~/.zshrc
