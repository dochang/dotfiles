dotfiles
========

`~/.dotfiles`

Installation
------------

Clone this repo, then run the following commands:

    ./bootstrap -i <inventory> [ other ansible arguments ... ]

Usage
-----

Every time you modify the configuration, run:

    ./bootstrap -i <inventory> [ other ansible arguments ... ]

Upgrade
-------

To upgrade all user space packages, pass `-e|--extra-vars pkg_state=latest` to
command line.
