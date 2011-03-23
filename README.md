My site lisp
============

Main files
----------

* `update-auto-loads.el`

Personal scripts
----------------

* `mars-tiling.el`
* `mars-windows-archiver.el`
* `revive+.el`

And all `compile*.sh` files you may need to compile the current directory.

Required packages
-----------------

===Include

* `bookmark-plus`

===Exclude

Those following packages aren't available here:

* `bbdb`
* `color-theme-6.6.0`
* `cedet` **IMPORTANT** CVS version, touch `.cedet` in the root
* `darcsum` (`darcs get --lazy -t http://joyful.com/repos/darcsum -t .`)
* `ecb` **IMPORTANT** touch `.noauto` in the root because this emacs uses `ecb-autoloads`
* `emacs-w3m`
* `emms` *git version*
* `haskellmode-emacs` (`darcs get http://code.haskell.org/haskellmode-emacs`)
* `howm-1.3.9.1`
* `keats` *git version*
* `magit` *git version*
* `mailcrypt-3.5.9`
* `markdown-mode` *git version*
* `mhc` *git version*
* `mu-cite-8.1`
* `newsticker-1.99`
* `nxhtml` *2.08-100425* **IMPORTANT** touch `.noauto` in the root because this emacs uses `nxhtml-loaddefs`
* `Pymacs` *git version*
* `python-mode`
* `remember` *git version*
* `rinari` *git version*
* `undo-tree` *git version*
* `vimpulse` *git version*
* `yasnippet` *SVN version*
* `yaml-mode` *git version* on yoshiki@gitub

But **they are auto-installed in the first directory of `mars/site-lisp-path` according to a simple command line scripting**. This destination should be `$HOME/.emacs.d/lisp` by default. If you experience problems with a package, you may replace it:

1. install the replacement package by hand in `~/.emacs.d/lisp`;
2. add it to your `load-path`;
3. remove the installer line of the previous version of the required package in `mars/site-lisp-packages` in `confs/packs.el`;
4. update autoloads (it should be made at startup if `loaddefs.el` isn't found in `~/.emacs.d/lisp`);
5. (optional) if the install process is simple and doable on the command line, add a line in `mars/site-lisp-packages` to check/build the new package in another environment.

Otherwise, you may want to simply update the erratic package by hand; for example, by relaunching the versioning tool via `git pull`, `svn up` or another command, and by byte-recompiling with `make`, `rake` or the included program `emacs-compile-directory`. A simple trick consists in removing the package directory and call interactively `mars/renew-site-lisp`. This last command will populate `~/.emacs.d/lisp` by:

1. checking if a required package is present;
2. install the required package if not found;
3. renew `load-path` AND FORCE TO GENERATE additional `autoloads` if newly installed.

In this case, the offending package will be re-installed and post-processed.

Moreover, `pases` should install then manage those packages:

* `wl-2.15.9` or an up-to-date *Wanderlust*
* `org-mode-7.5`

and the additional libraries used by `wl-2.15.9`:

* `apel-10.8`
* `flim-1.14.9_20100804`
* `semi-1.14.6_20101024`

`pases` permits the use of multiple major versions of **GNU Emacs** or **XEmacs** to work correctly.

===Tagging conventions

Add:
1. `.cedet` in the directory where the files and those ones in its subdirectories need a `cedet-autoload` (including `defclass`) instead of the standard *autoload* (normally, `cedet` and `ecb` directories should have one);
2. `.noauto` in the directory where the files and those ones in its subdirectories should not be seen via the `load-path` and should not have their *autoloads* generated and added to the `.emacs.d/lisp/loaddefs.el` file (here, `ecb`'s *autoloads* are loaded from the Emacs configuration so we don't have to visit the `ecb` directory);
3. `.nosearch` in the directory where the files and those ones in its subdirectories should not be seen via the `load-path`

Those tag-files are automatically added during the auto-installation of the non-`pases` packages.
