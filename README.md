Dotfiles
========

After cloning this repo, run `install` to automatically set up the development
environment. Note that the install script is idempotent - running it multiple
times has no effect.

Dotfiles uses [Dotbot][dotbot] for installation.

Screenshot
----------

![screenshot][screenshot]

Programs Used
-------------

### Command Line Tools

* `brew` (for Mac)
* `aptitude` (for Linux)
* `zsh`
* `tmux`
* `vim`
* `ctags` (exuberant)
* `git`
* `autojump`
* `axel`
* `rtorrent`

### Development Software

* `gcc` / `build-essential` package
* `python3`
* `sbt`
* `virtualenv`

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

Copyright (c) 2014 Anish Athalye. Released under the MIT License. See
[LICENSE.md][license] for details.

[screenshot]: misc/screenshots/2014-07-13.png
[dotbot]: https://github.com/anishathalye/dotbot
[license]: LICENSE.md
