# sandinmyjoints' .emacs.d

Works on Emacs >=23.

Some packages are git submodules, so after cloning, do:

`$ git submodule init && git submodule update`

Some packages are ELPA packages. On Emacs >=24, you'll be prompted to install
them automatically. On Emacs 23, you'll need to install
[package.el](http://bit.ly/pkg-el23) manually or with
[auto-install](http://www.emacswiki.org/emacs/auto-install.el) (which itself
must be installed manually).

One package, `dirtree`, requires a package, `windata`, that isn't in a package
repo or on github. It can be automatically installed from
[EmacsWiki](http://www.emacswiki.org/emacs/windata.el) with
[auto-install](http://www.emacswiki.org/emacs/auto-install.el).
