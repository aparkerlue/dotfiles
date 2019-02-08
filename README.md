[//]: # (-*- mode: Markdown; coding: utf-8; -*-)

# dotfiles #

Alan's dotfiles


## Setup ##

Install [GNU Stow](https://www.gnu.org/software/stow/) and clone the repository into `~/.dotfiles`.
Then, to deploy symlinks:

```sh
cd ~/.dotfiles
find * -maxdepth 0 -type d | xargs -d '\n' stow -v
```

I also keep some secret dotfiles in `~/Dropbox/dotfiles`. To deploy
those, I would run:

```sh
cd ~/Dropbox/dotfiles
stow -vt ~ *
```


## Tearing down ##

To remove symlinks:

```sh
cd ~/.dotfiles
find * -maxdepth 0 -type d | xargs -d '\n' stow -Dv
```

Similarly for dotfiles in `~/Dropbox/dotfiles`:

```sh
cd ~/Dropbox/dotfiles
stow -Dvt ~ *
```


## Choice of symlink deployment utility ##

I used to use [rcm](https://github.com/thoughtbot/rcm), but I switched to [GNU Stow](https://www.gnu.org/software/stow/) in June 2019
when I started using Debian and found that [apt.thoughtbot.com](https://apt.thoughtbot.com/)'s
SSL certificate had expired just a couple weeks earlier. I prefer
rcm's default behavior of creating directories and symlinking to files
(whereas Stow symlinks to a directory when it can), but trying to
problem-solve installing on Debian was just too much trouble.

I've saved rcm-specific information below.

### Configuration ###

`~/.rcrc`:

```bash
DOTFILES_DIRS="$HOME/.dotfiles $HOME/Dropbox/dotfiles"
EXCLUDES="LICENSE README.md"
```

### Helper functions ###

```sh
rcupdate() {
    git -C ~/.dotfiles pull &>/dev/null
}
```
