# Set custom prompt
autoload -U colors && colors
PS1=$'%{$fg[green]%}%6(c:.../:)%5c %{$fg[blue]%}\xe2\x9d\xb1 %{$reset_color%}'
PROMPTINFO="%(!.%{$fg[red]%}.%{$fg[cyan]%})%n%{$fg[magenta]%} at %{$fg[yellow]%}%m%{$reset_color%}"

# Initialize completion
autoload -Uz compinit && compinit

# Set automatic cd (typing directory name with no 'cd')
set autocd

# Colorize terminal
UNAMESTR=`uname`
if [[ "$UNAMESTR" == 'Darwin' ]]; then
  alias ls='ls -G'
  alias ll='ls -lG'
  alias la='ls -laG'
elif [[ "$UNAMESTR" == 'Linux' ]]; then
  alias ls='ls --color'
  alias ll='ls -l --color'
  alias la='ls -la --color'
fi
export LSCOLORS="ExGxBxDxCxEgEdxbxgxcxd"
export GREP_OPTIONS="--color"

# Nicer history
export HISTSIZE=4096
export HISTFILE="$HOME/.zsh_history"
export SAVEHIST=$HISTSIZE

# Use vim as the editor
export EDITOR=vi

# Use emacs mode in vim
bindkey -e

# Use incremental search
bindkey "^R" history-incremental-search-backward

# Jump to directory containing file
function jump() {
  cd "(dirname ${1})"
}

# Mirror a website
alias mirrorsite='wget -m -k -K -E -e robots=off'

# Set RHS prompt for git repositories
# Found at http://blog.joshdick.net/2012/12/30/my_git_prompt_for_zsh.html
# Adapted from code found at <https://gist.github.com/1712320>.

setopt prompt_subst

# Modify the colors and symbols in these variables as desired.
# GIT_PROMPT_SYMBOL="%{$fg[blue]%}±"
GIT_PROMPT_SYMBOL=""
GIT_PROMPT_PREFIX="%{$fg[green]%}[%{$reset_color%}"
GIT_PROMPT_SUFFIX="%{$fg[green]%}]%{$reset_color%}"
GIT_PROMPT_AHEAD="%{$fg[red]%}ANUM%{$reset_color%}"
GIT_PROMPT_BEHIND="%{$fg[cyan]%}BNUM%{$reset_color%}"
LIGHTNING="\xe2\x9a\xa1\xef\xb8\x8e"
GIT_PROMPT_MERGING="%{$fg_bold[magenta]%}${LIGHTNING}{$reset_color%}"
GIT_PROMPT_UNTRACKED="%{$fg_bold[red]%}*%{$reset_color%}"
GIT_PROMPT_MODIFIED="%{$fg_bold[yellow]%}*%{$reset_color%}"
GIT_PROMPT_STAGED="%{$fg_bold[green]%}*%{$reset_color%}"

# Show Git branch/tag, or name-rev if on detached head
parse_git_branch() {
  (git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null
}

# Show different symbols as appropriate for various Git repository states
parse_git_state() {

  # Compose this value via multiple conditional appends.
  local GIT_STATE=""

  local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_AHEAD" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD}
  fi

  local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_BEHIND" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND}
  fi

  local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
  if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
  fi

  if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_UNTRACKED
  fi

  if ! git diff --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MODIFIED
  fi

  if ! git diff --cached --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_STAGED
  fi

  if [[ -n $GIT_STATE ]]; then
    echo "$GIT_PROMPT_PREFIX$GIT_STATE$GIT_PROMPT_SUFFIX"
  fi

}

# If inside a Git repository, print its branch and state
git_prompt_string() {
  local git_where="$(parse_git_branch)"
  [ -n "$git_where" ] && echo " $GIT_PROMPT_SYMBOL$(parse_git_state)$GIT_PROMPT_PREFIX%{$fg[yellow]%}${git_where#(refs/heads/|tags/)}$GIT_PROMPT_SUFFIX"
}

# Set the right-hand prompt
RPS1='${PROMPTINFO}$(git_prompt_string)'

# Allow local customizations in the ~/.zshrc_local file
if [ -f ~/.zshrc_local ]; then
  source ~/.zshrc_local
fi
