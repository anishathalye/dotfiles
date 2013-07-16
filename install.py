#!/usr/bin/env python

# Script to link all the files in the dotfiles to the home directory

# link names are absolute (with '~' allowed, which will be expanded)
# target names are relative with respect to this python script
# target directories should have a trailing '/'
links = {
  # tmux
  '~/.tmux.conf': 'tmux.conf',
  
  # screen
  '~/.screenrc': 'screenrc',

  # vim
  '~/.vimrc': 'vimrc',
  '~/.vim': 'vim/',

  # zsh
  '~/.zshrc': 'zshrc',

  # git
  '~/.gitconfig': 'gitconfig',
  '~/.gitignore_global': 'gitignore_global'
}

####################

import os

def self_path():
  return os.path.dirname(os.path.realpath(__file__))

def exists(path):
  '''Returns true iff the path exists'''
  path = os.path.expanduser(path)
  return os.path.exists(path)

def islink(path):
  '''Returns true iff the path is a symlink'''
  return os.path.islink(os.path.expanduser(path))

def linkdest(path):
  '''Returns the absolute path to the destination of the symlink'''
  path = os.path.expanduser(path)
  reldest = os.readlink(path)
  return os.path.join(os.path.dirname(path), reldest)

class LinkingException(Exception):
  pass

def link(source, link_name):
  source = os.path.join(self_path(), source)
  if not exists(link_name) and islink(link_name):
    raise LinkingException('%s already exists but points to nonexistant %s' %
      (link_name, linkdest(link_name)))
  elif not exists(link_name):
    print '[*] creating link %s -> %s' % (link_name, source)
    os.symlink(source, os.path.expanduser(link_name))
  elif exists(link_name) and not islink(link_name):
    raise LinkingException('%s already exists but is a regular file' % link_name)
  elif not (linkdest(link_name) == source):
    raise LinkingException('%s already exists but points to the wrong file %s' %
      (link_name, linkdest(link_name)))
  else:
    print '[ ] link already exists from %s -> %s' % (link_name, source)

def main():
  unsuccessful = []
  for link_name in links:
    try:
      link(links[link_name], link_name)
    except LinkingException as e:
      print '[!] %s' % e.message
      unsuccessful.append(link_name)
  print '' # newline
  if unsuccessful:
    print 'ERROR: some links were not successfully set up:'
    print '\n'.join(['* %s' % i for i in unsuccessful])
  else:
    print 'SUCCESS: all links have been set up'

if __name__ == '__main__':
  main()
