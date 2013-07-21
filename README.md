dotfiles
========

After cloning this repo, run `install.py` to automatically set up the
development environment. Then run `git update-submodules` to install
the latest versions of all submodules. Note that the install script
must be run before updating submodules (because `update-submodules`
is not a vanilla git command but an alias set up by the installer).

To keep submodules up-to-date, you can re-run `git update-submodules`
at any time.

programs used
-------------

### command line tools

* `brew` (for mac)
* `aptitude` (for linux)
* `zsh`
* `tmux`
* `vim`
* `git`
* `autojump`

### development software

* `g++` / `build-essential` package
* `python3`
* `sbt`

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
