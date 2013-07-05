#!/bin/bash
# Script to link all the files in dotfiles to the home directory

BASEDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# tmux
ln -s ${BASEDIR}/tmux.conf ~/.tmux.conf

# vim
ln -s ${BASEDIR}/vimrc ~/.vimrc
ln -s ${BASEDIR}/vim/ ~/.vim

# zsh
ln -s ${BASEDIR}/zshrc ~/.zshrc
ln -s ${BASEDIR}/zshenv ~/.zshenv
ln -s ${BASEDIR}/zsh/ ~/.zsh
