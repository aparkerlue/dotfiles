# dotfiles #

Alan's dotfiles


## Setup ##

Install [rcm](https://github.com/thoughtbot/rcm).


## Configuration ##

`~/.rcrc`:

```bash
DOTFILES_DIRS="$HOME/.dotfiles $HOME/Dropbox/dotfiles"
EXCLUDES="LICENSE README.md"
```


## Helper functions ##

```sh
rcupdate() {
    git -C ~/.dotfiles pull &>/dev/null
}
```
