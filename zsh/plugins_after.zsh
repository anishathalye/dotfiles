# External plugins (initialized after)

# dircolors
if [[ "$(tput colors)" == "256" ]]; then
    eval $(dircolors =(cat ~/.zsh/plugins/dircolors-solarized/dircolors.256dark ~/.zsh/dircolors.extra))
fi
