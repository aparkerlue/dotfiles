# -*- mode: Shell-script; coding: utf-8; -*-

# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
umask 0022

# Initialize the search path
if [ -x /usr/libexec/path_helper ]; then
    export PATH=""
    eval $(/usr/libexec/path_helper -s)
else
    export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
fi

if [ -d "/snap/bin" ]; then
    PATH="/snap/bin:$PATH"
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
export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:'
export EDITOR="vim"
export TERMINAL="urxvt"
# export TERMINAL="sakura"
export TEXMFHOME="$XDG_DATA_HOME/texmf"

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

# Go
export GOPATH="$HOME/.go"
PATH="$GOPATH/bin:$PATH"

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
    if [ -n "$BASH_VERSION" ]; then
        export -f pyenv
    fi
fi

# pyenv-virtualenv - git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
if command -v pyenv &>/dev/null && [ -d "$(pyenv root)/plugins/pyenv-virtualenv" ]; then
    eval "$(pyenv virtualenv-init -)"
fi

# virtualenvwrapper - pip install virtualenvwrapper
export WORKON_HOME="$HOME/.virtualenvs"
export PROJECT_HOME="$HOME/src"
if [ -n "$BASH_VERSION" ] && pyenv which virtualenvwrapper.sh &>/dev/null; then
    source "$(pyenv which virtualenvwrapper.sh)"
fi

# npm
npm_packages=$HOME/.npm-packages
PATH="$npm_packages/bin:$PATH"
MANPATH="$npm_packages/share/man:$MANPATH"
NODE_PATH="$npm_packages/lib/node_modules"

# RubyGems
if [ -d "$HOME/.gem/ruby/2.5.0/bin" ]; then
    PATH="$HOME/.gem/ruby/2.5.0/bin:$PATH"
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
if /usr/libexec/java_home &>/dev/null; then
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
