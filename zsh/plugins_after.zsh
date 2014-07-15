# External plugins (initialized after)

# dircolors
if [[ "$(tput colors)" == "256" ]]; then
    eval $(dircolors ~/.zsh/plugins/dircolors-solarized/dircolors.256dark)
fi
