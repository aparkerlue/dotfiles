# -*- mode: sh; coding: utf-8; -*-

# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
umask 0002

# Initialize the search path
if [ -x /usr/libexec/path_helper ]; then
    export PATH=""
    eval $(/usr/libexec/path_helper -s)
else
    export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d ~/.profile.d ]; then
  for i in ~/.profile.d/*.sh; do
    if [ -r $i ]; then
      . $i
    fi
  done
  if [ -n "$BASH_VERSION" ]; then
      for i in ~/.profile.d/*.bash; do
          if [ -r $i ]; then
              . $i
          fi
      done
  fi
  unset i
fi

# Read .env
if [ -f ~/.env ]; then
    . ~/.env
fi

export MANPATH=":$HOME/man"
export CLICOLOR=1
export LC_COLLATE="C"
export EDITOR="vim"

# GnuPG and SSH agent
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"

# XDG user directories
if [ -z "$XDG_CONFIG_HOME" ]; then
    export XDG_CONFIG_HOME=$HOME/.config
fi
if [ -z "$XDG_CACHE_HOME" ]; then
    export XDG_CACHE_HOME=$HOME/.cache
fi
if [ -z "$XDG_DATA_HOME" ]; then
    export XDG_DATA_HOME=$HOME/.local/share
fi

# XDG system directories
if [ -z "$XDG_DATA_DIRS" ]; then
    export XDG_DATA_DIRS=/usr/local/share:/usr/share
fi
if [ -z "$XDG_CONFIG_DIRS" ]; then
    export XDG_CONFIG_DIRS=/etc/xdg
fi

# Poetry
if [ -d "$HOME/.poetry/bin" ]; then
    PATH="$HOME/.poetry/bin:$PATH"
fi

# Python user base directory
export PYTHONUSERBASE="${HOME}/.local"

# pyenv - git clone https://github.com/pyenv/pyenv.git ~/.pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
    export -f pyenv
fi

# pyenv-virtualenv - git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
if [ -d "$(pyenv root)/plugins/pyenv-virtualenv" ]; then
    eval "$(pyenv virtualenv-init -)"
fi

# virtualenvwrapper - pip install virtualenvwrapper
export WORKON_HOME="$HOME/.virtualenvs"
export PROJECT_HOME="$HOME/src"
source "$(pyenv which virtualenvwrapper.sh)"

# npm
npm_packages=$HOME/.npm-packages
PATH="$npm_packages/bin:$PATH"
MANPATH="$npm_packages/share/man:$MANPATH"
NODE_PATH="$npm_packages/lib/node_modules"

# RubyGems
if [ -d "$HOME/.gem/ruby/2.3.0/bin" ]; then
    PATH="$HOME/.gem/ruby/2.3.0/bin:$PATH"
fi

# Spark
export SPARK_HOME='/usr/local/Cellar/apache-spark/2.2.1/libexec'
export PYSPARK_PYTHON=python3
export PYSPARK_DRIVER_PYTHON=ipython

# pkg-config: ncurses
export PKG_CONFIG_PATH="/usr/local/opt/ncurses/lib/pkgconfig"

# lesspipe: make less more friendly for non-text input files, see
# lesspipe(1)
if [ -x /usr/bin/lesspipe ]; then
    eval "$(SHELL=/bin/sh lesspipe)"
fi

# macOS --------------------------------------------------------------

# brew info lesspipe
if [ -x /usr/local/bin/lesspipe.sh ]; then
    export LESSOPEN="|/usr/local/bin/lesspipe.sh %s" LESS_ADVANCED_PREPROCESSOR=1
fi

# Java
if [ -x /usr/libexec/java_home ]; then
    export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
    PATH="$JAVA_HOME/bin:$PATH"
fi

# .bashrc ----------------------------------------------------------------------

case $- in
    *i*)
        if [ -n "$BASH_VERSION" ] && [ -f "$HOME/.bashrc" ]; then
            . "$HOME/.bashrc"
        fi
        ;;
    *)
        ;;
esac
