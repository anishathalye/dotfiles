Dotfiles
========

After cloning this repo, run `install.py` to automatically set up the
development environment. Note that the install script is idempotent - running
it multiple times has no effect.

To keep submodules up-to-date, you can re-run `git update-submodules` at any
time.

Programs Used
-------------

### Command Line Tools

* `brew` (for mac)
* `aptitude` (for linux)
* `zsh`
* `tmux`
* `vim`
* `git`
* `autojump`

### Development Software

* `g++` / `build-essential` package
* `python3`
* `sbt`

Installing Binaries
-------------------

Use `aptitude` or whatever package manager on Linux. Use `brew` on Mac.

To replace coreutils with GNU coreutils on a Mac, do `brew install coreutils`
and follow the instructions to use the commands with their normal names.

Making Local Customizations
---------------------------

You can make local customizations for some programs by editing these files:

* `vim` : `~/.vimrc_local`
* `zsh` : `~/.zshrc_local_before` run before `.zshrc`
* `zsh` : `~/.zshrc_local_after` run after `.zshrc`
* `git` : `~/.gitconfig_local`
* `tmux` : `~/.tmux_local.conf`

License
-------

Released under the [MIT License](http://anish.mit-license.org/). See
`LICENSE.md` for details.
