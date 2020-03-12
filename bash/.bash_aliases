# -*- mode: Shell-script; coding: utf-8; -*-
if [[ $OSTYPE == darwin* ]]; then
     if command -v gls >/dev/null; then
         alias ls='gls --color=auto'
     else
         alias ls='ls -G'
     fi
else
    alias ls='ls --color=auto --quoting-style=shell'
fi
alias psudo='sudo env "PATH=$PATH"'
alias scrots='scrot -s "$HOME/Downloads/$(date +"%Y-%m-%d-%H%M%S")-\$wx\$h_scrot.png"'
