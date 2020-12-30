# -*- mode: shell-script; sh-shell: bash; coding: utf-8; -*-
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

# less
# GNU Source-highlight
# Requires: apt install source-highlight
src_hilite_lesspipe="/usr/share/source-highlight/src-hilite-lesspipe.sh"
if [ -x $src_hilite_lesspipe ]; then
    alias sless="LESSOPEN='| ${src_hilite_lesspipe} %s' less -R"
fi
unset src_hilite_lesspipe
# Pygments
pygmentize="/usr/bin/pygmentize"
if [ -x $pygmentize ]; then
    alias pless="LESSOPEN='| ${pygmentize} %s' less -R"
fi
unset pygmentize

alias scu='systemctl --user'
