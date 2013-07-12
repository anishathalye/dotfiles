dotfiles
========

Clone this repo and run `install.py` to configure your development environment.

installing binaries
-------------------

Use `aptitude` or whatever package manager on Linux. Use `brew` on Mac.

To replace coreutils with GNU coreutils on a Mac, do `brew install coreutils`
and follow the instructions to use the commands with their normal names.

making local customizations
---------------------------

You can make local customizations for some programs by editing these files:
* `vim` : `~/.vimrc_local`
* `zsh` : `~/.zshrc_local` for low-priority settings (run after `.zshrc`)
* `zsh` : `~/.zshrc_local_first` for high-priority settings (run before `.zshrc`)
* `git` : `~/.gitconfig_local`
* `tmux` : `~/.tmux_local.conf`
