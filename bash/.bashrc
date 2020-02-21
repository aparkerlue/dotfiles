# -*- mode: Shell-script; coding: utf-8; -*-

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

refresh-gpg-agent() {
    export GPG_TTY="$(tty)"
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    gpg-connect-agent updatestartuptty /bye
}

# Set shell prompt ---------------------------------------------------

set_ps1() {
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

    unset -f set_ps1
}

set_powerline() {
    # Powerline – https://github.com/powerline/powerline
    if command -v powerline-daemon >/dev/null; then
        powerline-daemon -q
        POWERLINE_BASH_CONTINUATION=1
        POWERLINE_BASH_SELECT=1
    fi
    . /usr/share/powerline/bindings/bash/powerline.sh

    unset -f set_powerline
}

is_ssh_session() {
    [[ -n $SSH_CLIENT || -n $SSH_CONNECTION || -n $SSH_TTY ]]
}

prefer_powerline=
if is_ssh_session || [[ -z $prefer_powerline ]]; then
    set_ps1
else
    set_powerline
fi
unset -f is_ssh_session set_ps1 set_powerline
unset -v prefer_powerline

# Git ----------------------------------------------------------------

git-branch-print-last-commit() {
    # Print remote branches with last commit on each branch.
    if ! git rev-parse --is-inside-working-tree >/dev/null; then
        return 128
    fi

    cat <<EOF
Branch                                  Last commit (<hash> <date> <author>)
--------------------------------------- ---------------------------------------
EOF
    git branch -r | sed '/HEAD/d' | while read b; do
        name=$(echo "$b" | sed 's#[^/]*/##')
        if [ $(echo "$name" | wc -c) -gt 39 ]; then
            name=$(echo "$name" | sed -E 's/(.{37}).*/\1../')
        fi
        commit=$(git log \
                -1 \
                --date=format:%Y-%m-%d \
                --pretty="format:%h %ad %<(20,trunc)%an" \
                "$b")
        printf '%-39s %-39s\n' "$name" "$commit"
    done | sort -k3
}

# Alias definitions
if [ -f "$HOME/.bash_aliases" ]; then
    . "$HOME/.bash_aliases"
fi
if [ -d ~/.aliases.d ]; then
    for f in ~/.aliases.d/*; do
        . "$f"
    done
    unset f
fi

# Key bindings
bind 'SPACE: magic-space'

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

for i in ~/.bash_completion.d/*; do
    if [[ -r $i ]]; then
        . $i
    fi
done

# Invoke ~/.bashrc.d/* scripts.
if [[ -d ~/.bashrc.d ]]; then
  for i in ~/.bashrc.d/*; do
    if [[ -r $i ]]; then
      . $i
    fi
  done
  unset i
fi

# direnv - brew install direnv
if command -v direnv >/dev/null; then
    eval "$(direnv hook bash)"
fi

# pyenv-virtualenv - git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
if command -v pyenv &>/dev/null && [ -d "$(pyenv root)/plugins/pyenv-virtualenv" ]; then
    eval "$(pyenv virtualenv-init -)"
fi

# virtualenvwrapper
if [ -z "$WORKON_HOME" ]; then
    export WORKON_HOME="$HOME/.virtualenvs"
fi
export PROJECT_HOME="$HOME/src"
if [ -n "$BASH_VERSION" ] && pyenv which virtualenvwrapper.sh &>/dev/null; then
    source "$(pyenv which virtualenvwrapper.sh)"
fi

# Pipenv - pip install pipenv
if command -v pipenv >/dev/null && pipenv --version &>/dev/null; then
    eval "$(pipenv --completion)"
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
