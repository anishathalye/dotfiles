Dotfiles
========

After cloning this repo, run `install` to automatically set up the
development environment. Note that the install script is idempotent - running
it multiple times has no effect.

To keep submodules up-to-date, you can re-run `git update-submodules` at any
time.

Dotfiles uses [Dotbot][2] for installation.

##### Note: You should edit `~/.gitconfig` to include your name and email.

Screenshot
----------

![screenshot][1]

Programs Used
-------------

### Command Line Tools

* `brew` (for mac)
* `aptitude` (for linux)
* `zsh`
* `tmux`
* `vim`
* `ctags` (exuberant)
* `git`
* `autojump`
* `axel`
* `rtorrent`

### Development Software

* `g++` / `build-essential` package
* `python3`
* `sbt`
* `node`
* `rbenv`

#### Ruby Gems

* `bundler`
* `rails`

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

With the exception of the content in directories that contain special
`LICENSE.md` files, all content is released under the MIT License. See
`LICENSE.md` for details.

[1]: https://github.com/anishathalye/dotfiles/raw/master/misc/screenshots/2014-03-20.png
[2]: https://github.com/anishathalye/dotbot
