# Allow local customizations in the ~/.zshrc_local_before file (executed first)
if [ -f ~/.zshrc_local_before ]; then
  source ~/.zshrc_local_before
fi

# Set custom prompt

# User customizable options
# RPR_SHOW_USER=(true, false)
# RPR_SHOW_HOST=(true, false) - show host in rhs prompt

# Load color variables to make it easier to color prompt
autoload -U colors && colors

# Make using 256 colors easier
if [[ "$(tput colors)" == "256" ]]; then
  source ~/.zsh/functions/spectrum.zsh
  # change default colors
  fg[green]=$FG[064]
  fg[cyan]=$FG[037]
  fg[blue]=$FG[033]
  fg[red]=$FG[160]
  fg[orange]=$FG[166]
  fg[yellow]=$FG[136]
  fg[magenta]=$FG[125]
  fg[violet]=$FG[061]
else
  fg[orange]=$fg[yellow]
  fg[violet]=$fg[magenta]
fi

# Current directory, truncated to 3 path elements (or 4 when one of them is "~")
function PR_DIR() {
  local full="$(print -P "%d")"
  local relfull="$(print -P "%~")"
  local shorter="$(print -P "%4~")"
  local current="$(print -P "%4(~:.../:)%3~")"
  local last="$(print -P "%1~")"

  # Longer path for '~/...'
  if [[ "${shorter}" == \~/* ]]; then
    current=${shorter}
  fi

  local truncated="$(echo "${current%/*}/")"

  # Handle special case of directory '/' or '~something'
  if [[ "${truncated}" == "/" || "${relfull[1,-2]}" != */* ]]; then
    truncated=""
  fi

  # Handle special case of last being '/...' one directory down
  if [[ "${full[2,-1]}" != "" && "${full[2,-1]}" != */* ]]; then
    truncated="/"
    last=${last[2,-1]} # take substring
  fi

  echo "%{$fg[green]%}${truncated}%{$fg[orange]%}%B${last}%b%{$reset_color%}"
}

# The arrow symbol that looks like > that is used in the prompt
PR_ARROW_CHAR=$(echo '\xe2\x9d\xb1')

# The arrow in red (for root) or violet (for regular user)
PR_ARROW="%(!.%{$fg[red]%}.%{$fg[violet]%})${PR_ARROW_CHAR}%{$reset_color%}"

# Build the prompt
PS1='$(PR_DIR) ${PR_ARROW} ' # space at the end

# Set custom rhs prompt
# User in red (for root) or violet (for regular user)
RPR_SHOW_USER=true # Set to false to disable user in rhs prompt
function RPR_USER() {
  if $RPR_SHOW_USER; then
    echo "%(!.%{$fg[red]%}.%{$fg[violet]%})%B%n%b%{$reset_color%}"
  fi
}

# Host in yellow
RPR_SHOW_HOST=true # Set to false to disable host in rhs prompt
function RPR_HOST() {
  if $RPR_SHOW_HOST; then
    echo "%{$fg[yellow]%}%m%{$reset_color%}"
  fi
}

# ' at ' in orange outputted only if both user and host enabled
function RPR_AT() {
  if $RPR_SHOW_USER && $RPR_SHOW_HOST; then
    echo "%{$fg[blue]%} at %{$reset_color%}"
  fi
}

# Build the rhs prompt
function RPR_INFO() {
  echo "$(RPR_USER)$(RPR_AT)$(RPR_HOST)"
}

# Initialize completion
autoload -Uz compinit && compinit

# Set automatic cd (typing directory name with no 'cd')
setopt autocd

# Use colors in coreutils utilities output
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias la='ls -la --color=auto'
if [[ "$(tput colors)" == "256" ]]; then
  eval $(dircolors ~/.zsh/other/dircolors.256dark)
fi
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

# cd to git root directory
alias git-root='cd "$(git rev-parse --show-toplevel)"'

# Jump to directory containing file
function jump() {
  cd "(dirname ${1})"
}

# cd replacement for screen to track cwd (like tmux)
scr_cd()
{
  builtin cd $1
  screen -X chdir $PWD
}

if [[ "$TERM" == 'screen.rxvt' ]]; then
  alias cd=scr_cd
fi

# Go up [n] directories
up()
{
  if [[ "${1}" == "" ]]; then
    cd ..
  elif ! [[ "${1}" =~ ^[0-9]+$ ]]; then
    echo "Error: argument must be a number"
  elif ! [[ "${1}" -gt "0" ]]; then
    echo "Error: argument must be positive"
  else
    for i in {1..${1}}; do
      cd ..
    done
  fi
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
GIT_PROMPT_PREFIX="%{$fg[green]%}%B(%b%{$reset_color%}"
GIT_PROMPT_SUFFIX="%{$fg[green]%}%B)%b%{$reset_color%}"
GIT_PROMPT_AHEAD="%{$fg[red]%}%B+NUM%b%{$reset_color%}"
GIT_PROMPT_BEHIND="%{$fg[blue]%}%-BNUM%b%{$reset_color%}"
LIGHTNING="\xe2\x9a\xa1\xef\xb8\x8e"
GIT_PROMPT_MERGING="%{$fg[orange]%}%B${LIGHTNING}%b%{$reset_color%}"
GIT_PROMPT_UNTRACKED="%{$fg[red]%}%B*%b%{$reset_color%}"
GIT_PROMPT_MODIFIED="%{$fg[yellow]%}%B*%b%{$reset_color%}"
GIT_PROMPT_STAGED="%{$fg[green]%}%B*%b%{$reset_color%}"

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
  [ -n "$git_where" ] && echo " $GIT_PROMPT_SYMBOL$(parse_git_state)$GIT_PROMPT_PREFIX%{$fg[red]%}%B${git_where#(refs/heads/|tags/)}%b$GIT_PROMPT_SUFFIX"
}

# Set the right-hand prompt
RPS1='$(RPR_INFO)$(git_prompt_string)'

# Syntax highlighting
source ~/.zsh/plugins/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
if [[ "$(tput colors)" == "256" ]]; then
  ZSH_HIGHLIGHT_STYLES[default]=none
  ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=160
  ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=037,bold #,standout
  ZSH_HIGHLIGHT_STYLES[alias]=fg=064,bold
  ZSH_HIGHLIGHT_STYLES[builtin]=fg=064,bold
  ZSH_HIGHLIGHT_STYLES[function]=fg=064,bold
  ZSH_HIGHLIGHT_STYLES[command]=fg=064,bold
  ZSH_HIGHLIGHT_STYLES[precommand]=fg=064,underline
  ZSH_HIGHLIGHT_STYLES[commandseparator]=none
  ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=037
  ZSH_HIGHLIGHT_STYLES[path]=fg=166,underline
  ZSH_HIGHLIGHT_STYLES[globbing]=fg=033
  ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=white,underline
  ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=fg=125,bold
  ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=fg=125,bold
  ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=none
  ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=136
  ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=136
  ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=136
  ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=136
  ZSH_HIGHLIGHT_STYLES[assign]=fg=037
fi

# Allow local customizations in the ~/.zshrc_local_after file
if [ -f ~/.zshrc_local_after ]; then
  source ~/.zshrc_local_after
fi
