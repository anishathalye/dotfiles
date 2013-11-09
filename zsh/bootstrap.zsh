if [[ "$(tput colors)" == "256" ]]; then
    eval $(dircolors ~/.zsh/plugins/dircolors-solarized/dircolors.256dark)
fi

# if rbenv is installed (and not an alias)
if rbenv_loc="$(type -p 'rbenv')" && ! [ -z "${rbenv_loc}" ]; then
    eval "$(rbenv init -)"
fi
