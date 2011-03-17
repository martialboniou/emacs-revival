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

Those packages aren't available here:

* `Pymacs`
* `bbdb`
* `circe`
* `bookmark-plus`
* `color-theme-6.6.0`
* `cedet` **IMPORTANT** CVS version, touch `.cedet` in the root
* `darcsum` (`darcs get --lazy -t http://joyful.com/repos/darcsum -t .`)
* `ecb` **IMPORTANT** touch `.noauto` in the root
* `emacs-w3m`
* `emms`
* `haskellmode-emacs` (`darcs get http://code.haskell.org/haskellmode-emacs`)
* `howm-1.3.8`
* `magit-0.8.2`
* `mailcrypt-3.5.9`
* `markdown-mode`
* `mhc`
* `mu-cite-8.1`
* `newsticker-1.99`
* `nxhtml-2.08-100425`
* `yasnippet` *SVN version*



Add:
1. `.cedet` in the directory where the files and those ones in its subdirectories need a `cedet-autoload` (including `defclass`) instead of the standard *autoload* (normally, `cedet` and `ecb` directories should have one);
2. `.noauto` in the directory where the files and those ones in its subdirectories should not be seen via the `load-path` and should not have their *autoloads* generated and added to the `.emacs.d/lisp/loaddefs.el` file (here, `ecb`'s *autoloads* are loaded from the Emacs configuration so we don't have to visit the `ecb` directory);
3. `.nosearch` in the directory where the files and those ones in its subdirectories should not be seen via the `load-path`
