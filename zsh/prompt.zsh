# User customizable options
# PR_ARROW_CHAR="[some character]"
# RPR_SHOW_USER=(true, false) - show username in rhs prompt
# RPR_SHOW_HOST=(true, false) - show host in rhs prompt
# RPR_SHOW_GIT=(true, false) - show git status in rhs prompt
# PR_EXTRA="[stuff]" - extra content to add to prompt
# RPR_EXTRA="[stuff]" - extra content to add to rhs prompt

# Set custom prompt

# Allow for variable/function substitution in prompt
setopt prompt_subst

# Load color variables to make it easier to color things
autoload -U colors && colors

# Make using 256 colors easier
if [[ "$(tput colors)" == "256" ]]; then
    source ~/.zsh/plugins/spectrum.zsh
    # change default colors
    fg[red]=$FG[160]
    fg[green]=$FG[064]
    fg[yellow]=$FG[136]
    fg[blue]=$FG[033]
    fg[magenta]=$FG[125]
    fg[cyan]=$FG[037]

    fg[teal]=$FG[041]
    fg[orange]=$FG[166]
    fg[violet]=$FG[061]
    fg[neon]=$FG[112]
    fg[pink]=$FG[183]
else
    fg[teal]=$fg[blue]
    fg[orange]=$fg[yellow]
    fg[violet]=$fg[magenta]
    fg[neon]=$fg[green]
    fg[pink]=$fg[magenta]
fi

# Current directory, truncated to 3 path elements (or 4 when one of them is "~")
# The number of elements to keep can be specified as ${1}
function PR_DIR() {
    local sub=${1}
    if [[ "${sub}" == "" ]]; then
        sub=3
    fi
    local len="$(expr ${sub} + 1)"
    local full="$(print -P "%d")"
    local relfull="$(print -P "%~")"
    local shorter="$(print -P "%${len}~")"
    local current="$(print -P "%${len}(~:.../:)%${sub}~")"
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

# An exclamation point if the previous command did not complete successfully
function PR_ERROR() {
    echo "%(?..%(!.%{$fg[violet]%}.%{$fg[red]%})%B!%b%{$reset_color%} )"
}

# The arrow symbol that is used in the prompt
PR_ARROW_CHAR=">"

# The arrow in red (for root) or violet (for regular user)
function PR_ARROW() {
    echo "%(!.%{$fg[red]%}.%{$fg[violet]%})${PR_ARROW_CHAR}%{$reset_color%}"
}

# Set custom rhs prompt
# User in red (for root) or violet (for regular user)
RPR_SHOW_USER=true # Set to false to disable user in rhs prompt
function RPR_USER() {
    if [[ "${RPR_SHOW_USER}" == "true" ]]; then
        echo "%(!.%{$fg[red]%}.%{$fg[violet]%})%B%n%b%{$reset_color%}"
    fi
}

function machine_name() {
    if [[ -f $HOME/.name ]]; then
        cat $HOME/.name
    else
        hostname
    fi
}

PROMPT_PYTHON="$(command -v python || command -v python3 || command -v python2)"

# Host in a deterministically chosen color
RPR_SHOW_HOST=true # Set to false to disable host in rhs prompt
function RPR_HOST() {
    local colors
    colors=(cyan green yellow red pink)
    local index=$("$PROMPT_PYTHON" <<EOF
import hashlib

hash = int(hashlib.sha1('$(machine_name)'.encode('utf8')).hexdigest(), 16)
index = hash % ${#colors} + 1

print(index)
EOF
    )
    local color=$colors[index]
    if [[ "${RPR_SHOW_HOST}" == "true" ]]; then
        echo "%{$fg[$color]%}$(machine_name)%{$reset_color%}"
    fi
}

# ' at ' in orange outputted only if both user and host enabled
function RPR_AT() {
    if [[ "${RPR_SHOW_USER}" == "true" ]] && [[ "${RPR_SHOW_HOST}" == "true" ]]; then
        echo "%{$fg[blue]%} at %{$reset_color%}"
    fi
}

# Build the rhs prompt
function RPR_INFO() {
    echo "$(RPR_USER)$(RPR_AT)$(RPR_HOST)"
}

# Set RHS prompt for git repositories
DIFF_SYMBOL="-"
GIT_PROMPT_SYMBOL=""
GIT_PROMPT_PREFIX="%{$fg[violet]%}%B(%b%{$reset_color%}"
GIT_PROMPT_SUFFIX="%{$fg[violet]%}%B)%b%{$reset_color%}"
GIT_PROMPT_AHEAD="%{$fg[teal]%}%B+NUM%b%{$reset_color%}"
GIT_PROMPT_BEHIND="%{$fg[orange]%}%B-NUM%b%{$reset_color%}"
GIT_PROMPT_MERGING="%{$fg[cyan]%}%Bx%b%{$reset_color%}"
GIT_PROMPT_UNTRACKED="%{$fg[red]%}%B$DIFF_SYMBOL%b%{$reset_color%}"
GIT_PROMPT_MODIFIED="%{$fg[yellow]%}%B$DIFF_SYMBOL%b%{$reset_color%}"
GIT_PROMPT_STAGED="%{$fg[green]%}%B$DIFF_SYMBOL%b%{$reset_color%}"
GIT_PROMPT_DETACHED="%{$fg[neon]%}%B!%b%{$reset_color%}"

# Show Git branch/tag, or name-rev if on detached head
function parse_git_branch() {
    (git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null
}

function parse_git_detached() {
    if ! git symbolic-ref HEAD >/dev/null 2>&1; then
        echo "${GIT_PROMPT_DETACHED}"
    fi
}

# Show different symbols as appropriate for various Git repository states
function parse_git_state() {
    # Compose this value via multiple conditional appends.
    local GIT_STATE="" GIT_DIFF=""

    local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
    if [ "$NUM_AHEAD" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD}
    fi

    local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
    if [ "$NUM_BEHIND" -gt 0 ]; then
        if [[ -n $GIT_STATE ]]; then
            GIT_STATE="$GIT_STATE "
        fi
    GIT_STATE=$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND}
    fi

    local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
    if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
        if [[ -n $GIT_STATE ]]; then
            GIT_STATE="$GIT_STATE "
        fi
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
    fi

    if [[ -n $(git ls-files --other --exclude-standard :/ 2> /dev/null) ]]; then
    GIT_DIFF=$GIT_PROMPT_UNTRACKED
    fi

    if ! git diff --quiet 2> /dev/null; then
    GIT_DIFF=$GIT_DIFF$GIT_PROMPT_MODIFIED
    fi

    if ! git diff --cached --quiet 2> /dev/null; then
    GIT_DIFF=$GIT_DIFF$GIT_PROMPT_STAGED
    fi

    if [[ -n $GIT_STATE && -n $GIT_DIFF ]]; then
        GIT_STATE="$GIT_STATE "
    fi
    GIT_STATE="$GIT_STATE$GIT_DIFF"

    if [[ -n $GIT_STATE ]]; then
    echo "$GIT_PROMPT_PREFIX$GIT_STATE$GIT_PROMPT_SUFFIX"
    fi
}

# If inside a Git repository, print its branch and state
RPR_SHOW_GIT=true # Set to false to disable git status in rhs prompt
function git_prompt_string() {
    if [[ "${RPR_SHOW_GIT}" == "true" ]]; then
        local git_where="$(parse_git_branch)"
        local git_detached="$(parse_git_detached)"
        [ -n "$git_where" ] && echo " $GIT_PROMPT_SYMBOL$(parse_git_state)$GIT_PROMPT_PREFIX%{$fg[magenta]%}%B${git_where#(refs/heads/|tags/)}%b$git_detached$GIT_PROMPT_SUFFIX"
    fi
}

PROMPT_MODE=0
PROMPT_MODES=4

# Function to toggle between prompt modes
function tog() {
    PROMPT_MODE=$(( (PROMPT_MODE + 1) % PROMPT_MODES))
}

function PR_EXTRA() {
    # do nothing by default
}

# Show select exported environment variables

_pr_var_list=()
_vars_multiline=false

function vshow() {
    local v
    for v in "$@"; do
        if [[ "${v}" =~ '[A-Z_]+' ]]; then
            if [[ ${_pr_var_list[(i)${v}]} -gt ${#_pr_var_list} ]]; then
                _pr_var_list+=("${v}")
            fi
        fi
    done
}

function vhide() {
    local v
    for v in "$@"; do
        _pr_var_list[${_pr_var_list[(i)${v}]}]=()
    done
}

function vmultiline() {
    if $_vars_multiline; then
        _vars_multiline=false
    else
        _vars_multiline=true
    fi
}

function PR_VARS() {
    local i v spc nl
    if $_vars_multiline; then
        spc=""
        nl="\n"
    else
        spc=" "
        nl=""
    fi
    for ((i=1; i <= ${#_pr_var_list}; i++)) do
        local v=${_pr_var_list[i]}
        if [[ -v "${v}" ]]; then
            # if variable is set
            if export | grep -Eq "^${v}="; then
                # if exported, show regularly
                printf '%s' "$spc%{$fg[cyan]%}${v}=${(P)${v}}%{$reset_color%}$nl"
            else
                # if not exported, show in red
                printf '%s' "$spc%{$fg[red]%}${v}=${(P)${v}}%{$reset_color%}$nl"
            fi
        fi
    done
    # show project-specific vars
    while read v; do
        if [[ "${v}" =~ '[A-Z_]+' ]]; then
            # valid environment variable
            if [[ ${_pr_var_list[(i)${v}]} -gt ${#_pr_var_list} ]]; then
                # not shown yet
                if export | grep -Eq "^${v}="; then
                    # exported
                    printf '%s' "$spc%{$fg[cyan]%}${v}=${(P)${v}}%{$reset_color%}$nl"
                fi
            fi
        fi
    done < <(git exec cat .showvars 2>/dev/null)
}

# Prompt
function PCMD() {
    if (( PROMPT_MODE == 0 )); then
        if $_vars_multiline; then
            echo "$(PR_VARS)$(PR_EXTRA)$(PR_DIR) $(PR_ERROR)$(PR_ARROW) " # space at the end
        else
            echo "$(PR_EXTRA)$(PR_DIR)$(PR_VARS) $(PR_ERROR)$(PR_ARROW) " # space at the end
        fi
    elif (( PROMPT_MODE == 1 )); then
        echo "$(PR_EXTRA)$(PR_DIR 1) $(PR_ERROR)$(PR_ARROW) " # space at the end
    else
        echo "$(PR_EXTRA)$(PR_ERROR)$(PR_ARROW) " # space at the end
    fi
}

PROMPT='$(PCMD)' # single quotes to prevent immediate execution
RPROMPT='' # set asynchronously and dynamically

function RPR_EXTRA() {
    # do nothing by default
}

# Right-hand prompt
function RCMD() {
    if (( PROMPT_MODE == 0 )); then
        echo "$(RPR_INFO)$(git_prompt_string)$(RPR_EXTRA)"
    elif (( PROMPT_MODE <= 2 )); then
        echo "$(git_prompt_string)$(RPR_EXTRA)"
    else
        echo "$(RPR_EXTRA)"
    fi
}

function precmd() {
    typeset -g _PROMPT_ASYNC_FD

    # close last fd, we don't care about the result anymore
    if [[ -n "$_PROMPT_ASYNC_FD" ]] && { true <&$_PROMPT_ASYNC_FD } 2>/dev/null; then
        exec {_PROMPT_ASYNC_FD}<&-
    fi

    # compute prompt in a background process
    exec {_PROMPT_ASYNC_FD}< <(printf "%s" "$(RCMD)")

    # when fd is readable, call response handler
    zle -F "$_PROMPT_ASYNC_FD" async_prompt_complete

    # do not clear RPROMPT, let it persist
}

function async_prompt_complete() {
    # read from fd
    RPROMPT="$(<&$1)"

    # remove the handler and close the fd
    zle -F "$1"
    exec {1}<&-

    # redisplay
    zle && zle reset-prompt
}
