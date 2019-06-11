# -*- mode: shell-script; coding: utf-8; -*-

refresh-gpg-agent() {
    export GPG_TTY="$(tty)"
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    gpg-connect-agent updatestartuptty /bye
}

set_PS1() {
    local RESET=$(tput sgr0 )
    local BOLD=$(tput bold )
    local RED=$(tput setaf 1 )
    local GREEN=$(tput setaf 2 )
    local YELLOW=$(tput setaf 3 )
    local BLUE=$(tput setaf 4 )
    local MAGENTABG=$(tput setab 5 )
    local CYAN=$(tput setaf 6 )

    local WHOAMI='\u'
    local WHERE='\w'
    local HOSTNAME='\h'
    local DATE='\D{%Y-%m-%d %H:%M:%S}'
    local LAST_RET='${?#0}'

    local LINE_1="$YELLOW$DATE $GREEN$WHOAMI$RED@$CYAN$HOSTNAME $BLUE$BOLD$WHERE$RESET"
    local LINE_2="\\[$MAGENTABG\\]$LAST_RET\\[$RESET\\]"'\$ '

    PS1="$LINE_1\n$LINE_2"

    unset -f set_PS1
}

set_PS1

# Alias definitions
if [ -f "$HOME/.bash_aliases" ]; then
    . "$HOME/.bash_aliases"
fi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# tmout
unset TMOUT

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

if ! shopt -oq posix; then
    # enable programmable completion features (you don't need to enable
    # this, if it's already enabled in /etc/bash.bashrc and /etc/profile
    # sources /etc/bash.bashrc).
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# direnv - brew install direnv
if command -v direnv >/dev/null; then
    eval "$(direnv hook bash)"
fi

# Pipenv - pip install pipenv
if command -v pipenv >/dev/null; then
    eval "$(pipenv --completion)"
fi

# Poetry
if command -v poetry >/dev/null; then
    eval "$(poetry completions bash)"
fi

# beorg
beorg="$HOME/Dropbox/org"

# GnuPG
gpgid="0xA5C2FE4660CF2A3D"

# macOS --------------------------------------------------------------

# brew info bash-completion@2
if type brew &>/dev/null \
        && [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]]; then
    export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d"
    . "/usr/local/etc/profile.d/bash_completion.sh"
fi

# iCloud Drive
icloud="$HOME/Library/Mobile Documents/com~apple~CloudDocs"

# npm ----------------------------------------------------------------

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
if [ -f $HOME/.npm-packages/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.bash ]; then
    . $HOME/.npm-packages/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.bash
fi
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
if [ -f $HOME/.npm-packages/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.bash ]; then
    . $HOME/.npm-packages/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.bash
fi