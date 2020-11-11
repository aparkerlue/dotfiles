# -*- mode: shell-script; coding: utf-8; -*-

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

if [ -d "$HOME/.cargo/bin" ]; then
    PATH="$HOME/.cargo/bin:$PATH"
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

# Ensure TMPDIR exists
if [ -n "${TMPDIR+set}" ]; then
    if [ -z "$TMPDIR" ]; then
        echo "warning: TMPDIR is set to the empty string" >&2
    elif [ ! -d "$TMPDIR" ]; then
        mkdir -m 0700 "$TMPDIR"
    fi
fi

# Disable flow control with C-s.
# stty -ixon

export INFOPATH="$HOME/.local/share/info:"
export MANPATH="$HOME/.local/share/man:"
export CLICOLOR=1
export LC_COLLATE="C"
export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:'

export EDITOR="emacsclient -nw"
export VISUAL="${EDITOR}"

# Avoid setting PAGER as long as it infers with Ledger:
# https://github.com/ledger/ledger/issues/1674
# export PAGER="less"

# Set TERMINAL for /usr/bin/i3-sensible-terminal.
if command -v alacritty >/dev/null; then
    export TERMINAL="alacritty"
elif command -v konsole >/dev/null; then
    export TERMINAL="konsole"
elif command -v gnome-terminal >/dev/null; then
    export TERMINAL="gnome-terminal"
elif command -v urxvt >/dev/null; then
    export TERMINAL="urxvt"
elif command -v sakura >/dev/null; then
    export TERMINAL="sakura"
fi
# Set BROWSER for /usr/bin/sensible-browser.
if command -v firefox-esr >/dev/null; then
    export BROWSER="firefox-esr"
elif command -v firefox >/dev/null; then
    export BROWSER="firefox"
elif command -v chromium >/dev/null; then
    export BROWSER="chromium"
elif command -v chromium-browser >/dev/null; then
    export BROWSER="chromium-browser"
fi

# GnuPG
export GPG_TTY="$(tty)"

export TEXMFHOME="$HOME/.local/share/texmf"

# Go
export GOPATH="$HOME/.go"
PATH="$GOPATH/bin:$PATH"

# Python user base directory
export PYTHONUSERBASE="${HOME}/.local"

# Poetry
if [ -d "$HOME/.poetry/bin" ]; then
    PATH="$HOME/.poetry/bin:$PATH"
fi

# npm
npm_packages=$HOME/.npm-packages
PATH="$npm_packages/bin:$PATH"
MANPATH="$npm_packages/share/man:${MANPATH}"
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

# .bashrc ------------------------------------------------------------

case $- in
    *i*)
        if [ -n "$BASH_VERSION" ] && [ -f "$HOME/.bashrc" ]; then
            . "$HOME/.bashrc"
        fi
        ;;
    *)
        ;;
esac

# nix ----------------------------------------------------------------

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh
fi
