#!/bin/sh
# Script to link all the files in dotfiles to the home directory

BASEDIR=$(dirname $0)
echo $BASEDIR

# ln -s ${BASEDIR}/.conf.file ~

ln -s ${BASEDIR}/tmux.conf ~/.tmux.conf
