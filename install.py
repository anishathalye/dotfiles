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
    '~/.zsh': 'zsh/',

    # git
    '~/.gitconfig': 'gitconfig',
    '~/.gitignore_global': 'gitignore_global',

    # sbt
    '~/.sbt': 'sbt/'
}

# shell commands (array of (msg, cmd))

# shell commands to run before linking
precmds = []

# shell commands to run after linking
postcmds = [
    ('Installing/updating submodules', 'git update-submodules')
]

####################

import sys, os, subprocess

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
        print '[!] invalid link %s -> %s' % \
            (link_name, linkdest(link_name))
        raise LinkingException()
    elif not exists(link_name):
        print '[*] creating link %s -> %s' % (link_name, source)
        os.symlink(source, os.path.expanduser(link_name))
    elif exists(link_name) and not islink(link_name):
        print '[!] %s already exists but is a regular file or directory' % link_name
        raise LinkingException()
    elif not (linkdest(link_name) == source):
        print '[!] incorrect link %s -> %s' % \
            (link_name, linkdest(link_name))
        raise LinkingException()
    else:
        print '[ ] link exists %s -> %s' % (link_name, source)

def process_links():
    unsuccessful = []
    for link_name in links:
        try:
            link(links[link_name], link_name)
        except LinkingException:
            unsuccessful.append(link_name)
    print '' # newline
    if unsuccessful:
        print 'FAILURE: some links were not successfully set up:'
        print '\n'.join(['* %s' % i for i in unsuccessful])
    else:
        print 'SUCCESS: all links have been set up'
    return bool(unsuccessful)

def process_shell(cmds):
    if not cmds:
        return False
    unsuccessful = False
    for msg, cmd in cmds:
        print '%s [%s]...' % (msg, cmd), # comma to avoid newline
        sys.stdout.flush() # force printing of above line
        ret = subprocess.call(cmd, shell = True, stdout = subprocess.PIPE,
            stderr = subprocess.PIPE)
        print '%s!' % ('SUCCESS' if ret == 0 else 'FAILURE')
        if ret != 0: unsuccessful = True
    print '' # newline
    if unsuccessful:
        print 'FAILURE: some tasks were not run successfully'
    else:
        print 'SUCCESS: all tasks executed'


def main():
    pre_fail = process_shell(precmds)
    if precmds: print '' # newline
    link_fail = process_links()
    if postcmds: print '' # newline
    post_fail = process_shell(postcmds)
    print '' # newline
    if any((pre_fail, link_fail, post_fail)):
        print 'FAILURE: environment has not been set up successfully'
    else:
        print 'SUCCESS: environment has been set up'

if __name__ == '__main__':
    main()
