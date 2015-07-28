dotfiles
========

`~/.dotfiles`

Installation
------------

    curl -sSL get.freshshell.com | FRESH_LOCAL_SOURCE=dochang/dotfiles bash

Then exit and re-login, run:

    ~/.dotfiles/bootstrap

Usage
-----

    fresh [update]
    battle --ask-sudo-pass

Upgrade
-------

To upgrade all user space packages, pass `-e|--extra-vars pkg_state=latest` to
command line.
