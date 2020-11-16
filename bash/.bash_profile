# -*- mode: shell-script; sh-shell: bash; coding: utf-8; -*-

# Load ~/.profile ----------------------------------------------------

if [ -r ~/.profile ]; then
    . ~/.profile
fi

# Execute ~/.bashrc if interactive -----------------------------------

case "$-" in
    *i*)
        if [ -r ~/.bashrc ]; then
            . ~/.bashrc
        fi;;
    *) ;;
esac
