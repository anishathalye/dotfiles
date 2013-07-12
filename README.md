dotfiles
========

Clone this repo and run `install.py` to configure your development environment.

programs used
-------------

Here is a list of the main programs used:

* `brew` (for mac)
* `aptitude` (for linux)
* `zsh`
* `tmux`
* `vim`
* `git`
* `autojump`

* `sbt`
* `python3`
* `g++`

installing binaries
-------------------

Use `aptitude` or whatever package manager on Linux. Use `brew` on Mac.

To replace coreutils with GNU coreutils on a Mac, do `brew install coreutils`
and follow the instructions to use the commands with their normal names.

making local customizations
---------------------------

You can make local customizations for some programs by editing these files:
* `vim` : `~/.vimrc_local`
* `zsh` : `~/.zshrc_local_before` run before `.zshrc`
* `zsh` : `~/.zshrc_local_after` run after `.zshrc`
* `git` : `~/.gitconfig_local`
* `tmux` : `~/.tmux_local.conf`
