# -*- mode: Shell-script; coding: utf-8; -*-
if [[ $OSTYPE == darwin* ]]; then
     if command -v gls >/dev/null; then
         alias ls='gls --color=auto'
     else
         alias ls='ls -G'
     fi
else
    alias ls='ls --color=auto'
fi
alias ec='emacsclient'
alias R='R -q --no-save'
alias per='pipenv run'
alias pri='pipenv run ipython'
alias prjc='pipenv run jupyter console'
alias prjl='pipenv run jupyter lab'
alias prjn='pipenv run jupyter notebook'
alias prjq='pipenv run jupyter qtconsole'
alias prp='pipenv run python'
