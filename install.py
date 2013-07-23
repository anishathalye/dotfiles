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

# colors
class colors:
    __tty = sys.stdout.isatty()
    NONE = '' if __tty else ''
    MAGENTA = '\033[95m' if __tty else ''
    YELLOW = '\033[93m' if __tty else ''
    BLUE = '\033[94m' if __tty else ''
    GREEN = '\033[92m' if __tty else ''
    RED = '\033[91m' if __tty else ''
    RESET = '\033[0m' if __tty else ''

    INFO = NONE
    OK = BLUE
    SUCCESS = GREEN
    WARNING = MAGENTA
    DETAIL = YELLOW
    FAIL = RED
    ENDC = RESET


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
        print colors.WARNING + \
            '[!] invalid link %s -> %s' % (link_name, linkdest(link_name)) + \
            colors.ENDC
        raise LinkingException()
    elif not exists(link_name):
        print colors.OK + \
            '[*] creating link %s -> %s' % (link_name, source) + colors.ENDC
        os.symlink(source, os.path.expanduser(link_name))
    elif exists(link_name) and not islink(link_name):
        print colors.WARNING + \
            '[!] %s already exists but is a regular file or directory' % \
            link_name + colors.ENDC
        raise LinkingException()
    elif not (linkdest(link_name) == source):
        print colors.WARNING + '[!] incorrect link %s -> %s' % \
            (link_name, linkdest(link_name)) + colors.ENDC
        raise LinkingException()
    else:
        print colors.INFO + '[ ] link exists %s -> %s' % \
            (link_name, source) + colors.ENDC

def process_links():
    unsuccessful = []
    for link_name in links:
        try:
            link(links[link_name], link_name)
        except LinkingException:
            unsuccessful.append(link_name)
    print '' # newline
    if unsuccessful:
        print colors.FAIL + \
            'FAILURE: some links were not successfully set up' + colors.ENDC
        # print colors.FAIL + \
        #    'FAILURE: some links were not successfully set up:' + colors.ENDC
        # print colors.WARNING + '\n'.join(['* %s' % i for i in unsuccessful]) + \
        #    colors.ENDC
    else:
        print colors.SUCCESS + 'SUCCESS: all links have been set up' + \
            colors.ENDC
    return bool(unsuccessful)

def process_shell(cmds):
    if not cmds:
        return False
    unsuccessful = False
    for msg, cmd in cmds:
        print colors.INFO + '%s [%s]...' % (msg,
            colors.DETAIL + cmd + colors.ENDC) + \
            colors.ENDC, # comma to avoid newline
        sys.stdout.flush() # force printing of above line
        ret = subprocess.call(cmd, shell = True, stdout = subprocess.PIPE,
            stderr = subprocess.PIPE)
        print '%s!' % (colors.OK + 'SUCCESS' if ret == 0 else
            colors.WARNING + 'FAILURE') + colors.ENDC
        if ret != 0: unsuccessful = True
    print '' # newline
    if unsuccessful:
        print colors.FAIL + 'FAILURE: some tasks were not run successfully' + \
            colors.ENDC
    else:
        print colors.SUCCESS + 'SUCCESS: all tasks executed' + colors.ENDC


def main():
    pre_fail = process_shell(precmds)
    if precmds: print '' # newline
    link_fail = process_links()
    if postcmds: print '' # newline
    post_fail = process_shell(postcmds)
    print '' # newline
    if any((pre_fail, link_fail, post_fail)):
        print colors.FAIL + \
            'FAILURE: environment has not been set up successfully' + \
            colors.ENDC
    else:
        print colors.SUCCESS + 'SUCCESS: environment has been set up' + colors.ENDC

if __name__ == '__main__':
    main()
