# -*- mode: shell-script; sh-shell: bash; coding: utf-8; -*-

# File mode creation mask --------------------------------------------

umask 0022

# Export environment.d configuration ---------------------------------

generate-environment() {
    systemd_environment_d_generator=/usr/lib/systemd/user-environment-generators/30-systemd-environment-d-generator
    if [ -x $systemd_environment_d_generator ]; then
        $systemd_environment_d_generator
    else
        echo "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
    fi
}
while read -r vardef; do
    export "$vardef"
done <<< $(generate-environment)
unset -v vardef
unset -f generate-environment

# Environment variables ----------------------------------------------

path-contains() {
    # Return zero status if path contains the given argument.
    echo "$PATH" | tr : '\n' | grep -q "^${1}\$"
}
prepend-to-path-if-eligible() {
    # Prepend to path if directory exists and is not already in path.
    dir=$(realpath "$1" 2>/dev/null)
    if [ $? -eq 0 ] && [ -d "$dir" ] && ! path-contains "$dir"; then
        echo "${dir}:${PATH}"
        return 0
    else
        echo "${PATH}"
        return 1
    fi
}

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
PATH=$(prepend-to-path-if-eligible $GOPATH/bin)

# Python user base directory
export PYTHONUSERBASE="${HOME}/.local"

# npm
npm_packages=$HOME/.npm-packages
PATH=$(prepend-to-path-if-eligible $npm_packages/bin)
MANPATH="$npm_packages/share/man:${MANPATH}"
NODE_PATH="$npm_packages/lib/node_modules"
unset -v npm_packages

PATH=$(prepend-to-path-if-eligible $HOME/.poetry/bin)
PATH=$(prepend-to-path-if-eligible $HOME/.cargo/bin)
PATH=$(prepend-to-path-if-eligible $HOME/.local/bin)
PATH=$(prepend-to-path-if-eligible $HOME/bin)

unset -f path-contains prepend-to-path-if-eligible

# ~/.profile.d files -------------------------------------------------

if [ -d ~/.profile.d ]; then
    for f in ~/.profile.d/*; do
        . "$f"
    done
fi
unset -v f

# Execute ~/.bashrc if interactive -----------------------------------

case "$-" in
    *i*) . ~/.bashrc ;;
    *) ;;
esac
