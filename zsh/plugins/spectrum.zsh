#! /bin/zsh
# A script to make using 256 colors in zsh less painful.
# P.C. Shyamshankar <sykora@lucentbeing.com>
# Copied from http://github.com/sykora/etc/blob/master/zsh/functions/spectrum/

typeset -Ag FX FG BG

FX=(
    reset     "%{[00m%}"
    bold      "%{[01m%}" no-bold      "%{[22m%}"
    italic    "%{[03m%}" no-italic    "%{[23m%}"
    underline "%{[04m%}" no-underline "%{[24m%}"
    blink     "%{[05m%}" no-blink     "%{[25m%}"
    reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
    FG[$color]="%{[38;5;${color}m%}"
    BG[$color]="%{[48;5;${color}m%}"
done

# Show all 256 colors with color number
function spectrum() {
    local cols=4
    if [[ "${1}" != "" ]]; then
        cols=${1}
    fi
    local ctr=1
    for code in {000..255}; do
        print -P -n -- "$code: %F{$code}Test%f"
        if [[ "$(expr ${ctr} % ${cols})" == "0" ]]; then
            print # newline
        else
            print -n -- " " # gap
        fi
        ctr=$(expr ${ctr} + 1)
    done
    print # newline
}
