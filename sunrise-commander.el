;; sunrise-commander.el  ---  Two-pane file manager for Emacs based on Dired and
;; inspired by MC.

;; Copyright (C) 2007 2008 2009 José Alfredo Romero Latouche (j0s3l0)

;; Author: José Alfredo Romero L. <joseito@poczta.onet.pl>
;; Keywords: Sunrise Commander Emacs File Manager Midnight Norton Orthodox

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation,  either  version  3 of the License, or (at your option) any later
;; version.
;;
;; This  program  is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR  A  PARTICULAR  PURPOSE.  See the GNU General Public License for more de-
;; tails.

;; You  should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Here  is  another two-pane mc emulation layer for emacs. It's built on top of
;; Dired and takes advantage of all its features, offering at the same time  the
;; double  pane  interface I'd been missing so badly since I started using regu-
;; larly emacs (for everything!). I tried  both  Ilya  Zakharevich's  nc.el  and
;; Kevin  Burton's  mc.el,  but  none of them was what I was looking for (though
;; mc.el was near the ideal).

;; A  lot  of  this code has been shamelessly copied from Kevin's mc.el and only
;; slightly modified. Another part of it - the code for recursive  file  copying
;; and  renaming - was adapted from the dired extensions written by Kurt Nørmark
;; for LAML (http://www.cs.aau.dk/~normark/scheme/distribution/laml/).

;; I have added to the mix several useful functions:

;; *  Sunrise  is  implemented  as a derived major mode confined inside the pane
;; buffers, so its buffers and dired ones can live together without easymenu  or
;; viper to avoid key binding collisions.

;; *  It  automatically  closes unused buffers and tries to never keep open more
;; than the one or two used to display the panes.

;; *  Each pane has its own history ring: press M-y / M-u for moving backwards /
;; forwards in the history of directories.

;; * Press C-= for "smart" file comparison using ediff. It compares together the
;; first two files marked on each pane or, if no files have been marked, it  as-
;; sumes that the second pane contains a file with the same name as the selected
;; one and tries to compare these two. You can also mark whole lists of files to
;; be compared and then just press C-= for comparing the next pair.

;; *  Press  = for fast "smart" file comparison -- like above, but using regular
;; diff.

;; * Press C-M-= for directory comparison (by date / size / contents of files).

;; * Press C-c t to open a terminal into the current pane's directory.

;; * Press M-t to swap the panes.

;; * Press C-c C-s to change the layout of the panes (horizontal/vertical/top)

;; *  Press  C-x C-q   to put the current pane in Editable Dired mode (allows to
;; edit the pane as if it were a regular file -- press C-c C-c  to  commit  your
;; changes to the filesystem, or C-c C-k to abort).

;; *  Sunrise VIRTUAL mode integrates dired-virtual mode to Sunrise, allowing to
;; capture find and locate results in regular files and to use them later as  if
;; they  were  directories  with  all  Dired  and  Sunrise  operations  at  your
;; fingertips.
;; The results of the following operations are displayed in VIRTUAL mode:
;;    - find-dired-name (press C-c C-n),
;;    - find-grep-name  (press C-c C-g),
;;    - find-dired      (press C-c C-f),
;;    - locate          (press C-c C-l),
;;    - list all recently visited files (press C-c C-r -- requires recentf),
;;    - list all directories in active pane's history ring (press C-c C-d).

;; * Supports AVFS (http://www.inf.bme.hu/~mszeredi/avfs/) for transparent navi-
;; gation inside compressed archives (*.zip, *.tgz, *.tar.bz2, *.deb, etc. etc.)
;; You  need to have AVFS with coda or fuse installed and running on your system
;; for this to work, though.

;; *  Terminal  integration  and Command line expansion: integrates tightly with
;; eshell or term-mode to allow interaction between terminal emulators  in  line
;; mode  (C-c  C-j)  and  the panes: the most important navigation commands (up,
;; down, mark, unmark, go to parent dir) can be  executed  on  the  active  pane
;; directly  from  the  terminal  by  pressing the usual keys with Meta: <M-up>,
;; <M-down>,  etc.  Additionally,  the following substitutions are automagically
;; performed in term-line-mode:
;;     %f - expands to the currently selected file in the left pane
;;     %F - expands to the currently selected file in the right pane
;;     %m - expands to the list of paths of all marked files in the left pane
;;     %M - expands to the list of paths of all marked files in the right pane
;;     %n - expands to the list of names of all marked files in the left pane
;;     %N - expands to the list of names of all marked files in the right pane
;;     %d - expands to the current directory in the left pane
;;     %D - expands to the current directory in the right pane

;; * Passive navigation: the usual navigation keys (n, p, Return, U, ;) combined
;; with Meta allow to move across the passive pane without  actually  having  to
;; switch to it.

;; * Synchronized navigation:  press  C-c C-z  to enable / disable  synchronized
;; navigation. In this mode, the passive navigation keys  (M-n,  M-p,  M-Return,
;; etc.)  operate on both panes simultaneously. I've found this quite useful for
;; comparing hierarchically small to medium-sized directory trees (for large  to
;; very  large  directory  trees  one  needs  something  on the lines of diff -r
;; though).

;; * etc. ;-)

;; It  doesn't  even  try to look like MC, so the help window is gone (you're in
;; emacs, so you know your bindings, right?), though if you really  miss it just
;; get and install the sunrise-x-buttons extension.

;; This is version 3 $Rev: 196 $ of the Sunrise Commander.

;; It  was  written  on GNU Emacs 23 on Linux, and tested on GNU Emacs 22 and 23
;; for Linux and on EmacsW32 (version 22) for  Windows.  I  have  also  received
;; feedback  from a user reporting it works OK on the Mac (GNU Emacs 22.2 on Mac
;; OS X Leopard). I *am* aware that  there  are  several  functions  (including,
;; alas,  file  and directory comparison) that simply will not work on GNU Emacs
;; 21, but unfortunately I do not have the time to port them back. I don't  know
;; either  if  it will work at all on XEmacs (uses overlays), so try at your own
;; risk. All contributions and/or bug reports will be very welcome.

;;; Installation and Usage:

;; 1) Put this file somewhere in your emacs load-path.

;; 2) Add a (require 'sunrise-commander) to your .emacs file.

;; 3) If you want the function keys bound to the usual MC commands (F5 for copy,
;; F6 for rename, and so on) add: (sunrise-mc-keys)  after  the  "require"  line
;; (IMHO  these  bindings  are not optimal for emacs, but I'm including them be-
;; cause some MC power users may have them too deeply embedded in  their  spinal
;; cord)

;; 4)  Choose  some  unused  extension for files to be opened in Sunrise VIRTUAL
;; mode and add it to auto-mode-alist, e.g. if you want  to  name  your  virtual
;; directories  like  *.svrm  just  add  to  your  .emacs  file  a line like the
;; following:
;;
;;     (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))

;; 5) Evaluate the new lines, or reload your .emacs file, or restart emacs.

;; 6) Type M-x sunrise to invoke the Sunrise Commander (or much better: bind the
;; function to your favorite key combination). The  command  sunrise-cd  invokes
;; Sunrise  and  automatically  selects  the  current file wherever it is in the
;; filesystem. Type h at any moment for information on available key bindings.

;; 7)  Type  M-x customize-group <RET> sunrise <RET> to customize options, fonts
;; and colors (activate AVFS support here, too).

;; 8) Enjoy :)

;;; Code:

(require 'dired)
(require 'dired-x)
(require 'font-lock)
(eval-when-compile (require 'cl))
(eval-when-compile (require 'esh-mode))
(eval-when-compile (require 'recentf))
(eval-when-compile (require 'term))

(defgroup sunrise nil
  "The Sunrise Commander File Manager."
  :group 'files)

(defcustom sr-show-file-attributes t
  "Whether to initially display file attributes in Sunrise panes. You can always
  toggle file attributes display pressing C-Backspace"
  :group 'sunrise
  :type 'boolean)

(defcustom sr-show-hidden-files nil
  "Whether  to  initially  display hidden files in Sunrise panes. You can always
  toggle hidden files display pressing C-o. You can also  customize  what  files
  are considered hidden by setting dired-omit-files and dired-omit-extensions in
  your .emacs file"
  :group 'sunrise
  :type 'boolean)

(defcustom sr-terminal-program "eshell"
  "The program to use for terminal emulation. If this value is set to
  \"eshell\", the emacs shell will be used."
  :group 'sunrise
  :type 'string)

(defcustom sr-listing-switches "-alp"
  "Listing  switches  to  use  (instead  of dired-listing-switches) for building
  Sunrise buffers. Notice that buffers in  VIRTUAL  mode  (such  as  the  recent
  directories and recent files listings) use sr-virtual-listing-switches instead
  of this.
  Most portable value: -alp
  Recommended value on GNU systems: \
--time-style=locale --group-directories-first -alDphgG"
  :group 'sunrise
  :type 'string)

(defcustom sr-virtual-listing-switches "--time-style=long-iso --group-directories-first -aldp"
  "Listing switches for building buffers in Sunrise VIRTUAL mode based on find
  and locate results. Sorting support in sr-virtual buffers depend on the
  correct format of their entries.

  NOTE:  the  default  value  for  these  switches may be incompatible with your
  installment if you're using Sunrise in  a  non-GNU  environment.  If  you  are
  getting error reports of the form ``Listing directory failed but `access-file'
  worked'', then you may try changing this value to -alp (be aware, though, that
  this will cripple sorting in all your VIRTUAL buffers), or much better install
  GNU coreutils in your system and use gls as your ls program.  For  details  on
  how to do this please consult the GNU Emacs FAQ for MS Windows.
  (Thanks Vagn Johansen for pointing out this issue)"
  :group 'sunrise
  :type 'string)

(defcustom sr-avfs-root nil
  "The root of the AVFS virtual filesystem to use for navigating compressed
   archives. Setting this value activates AVFS support."
  :group 'sunrise
  :type '(choice
          (const :tag "AVFS support disabled" nil)
          (directory :tag "AVFS root directory")))

(defcustom sr-avfs-handlers-alist '(("\\.[jwesh]ar$" . "#uzip/")
                                    ("\\.xpi$"       . "#uzip/")
                                    ("."             . "#/"))
  "List of AVFS handlers to manage specific file extensions."
  :group 'sunrise
  :type 'alist)

(defcustom sr-md5-shell-command "md5sum %f | cut -d' ' -f1 2>/dev/null"
  "Shell command to use for calculating MD5 sums for files when comparing
   directories using the ``(c)ontents'' option. Use %f as a placeholder for the
   name of the file."
  :group 'sunrise
  :type 'string)

(defcustom sr-window-split-style 'horizontal
  "The current window split configuration.  May be 'horizontal, 'vertical or 'top"
  :group 'sunrise
  :type '(choice
          (const horizontal)
          (const vertical)
          (const top)))

(defcustom sr-windows-locked t
  "Flag that indicates whether the vertical size of the panes should remain
  constant during Sunrise operation."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-history-length 20
  "Number of entries to keep in each of the pane history rings."
  :group 'sunrise
  :type 'integer)

(defcustom sr-start-hook nil
  "List of functions to be called after the Sunrise panes are displayed"
  :group 'sunrise
  :type 'hook
  :options '(auto-insert))

(defcustom sr-quit-hook nil
  "List of functions to be called after the Sunrise panes are hidden"
  :group 'sunrise
  :type 'hook
  :options '(auto-insert))

(defvar sr-restore-buffer nil
  "Buffer to restore when sr quits.")

(defvar sr-prior-window-configuration nil
  "Window configuration before sr was started.")

(defvar sr-running nil "True when sr commander mode is running.")

(defvar sr-synchronized nil "True when synchronized navigation is on")

(defvar sr-current-window-overlay nil
  "Holds the current overlay which marks the current dired buffer.")

(defvar sr-clex-hotchar-overlay nil
  "Holds the overlay used to highlight the hot character (%) during CLEX
  operations.")

(defvar sr-left-directory "~/"
  "Dired directory for the left window.  See variable `dired-directory'.")

(defvar sr-left-buffer nil
  "Dired buffer for the left window.")

(defvar sr-left-window nil
  "The left window of dired.")

(defvar sr-right-directory "~/"
  "Dired directory for the right window.  See variable `dired-directory'.")

(defvar sr-right-buffer nil
  "Dired buffer for the right window.")

(defvar sr-right-window nil
  "The right window of dired.")

(defvar sr-current-frame nil
  "The frame Sunrise is active on (if any)")

(defvar sr-this-directory "~/"
  "Dired directory in the active pane. This isn't necessarily the same as
  dired-directory")

(defvar sr-other-directory "~/"
  "Dired directory in the passive pane")

(defvar sr-selected-window 'left
  "The window to select when sr starts up.")

(defvar sr-history-registry
  (acons 'left nil (acons 'right nil nil))
  "Registry of visited directories for both panes")

(defvar sr-checkpoint-registry
  (acons "~" (list sr-left-directory sr-right-directory) nil)
  "Registry of currently defined checkpoints")

(defvar sr-ti-openterms nil
  "Stack of currently open terminal buffers")

(defvar sr-ediff-on nil
  "Flag that indicates whether an ediff is being currently done")

(defvar sr-clex-on nil
  "Flag that indicates that a CLEX operation is taking place")

(defvar sr-virtual-buffer nil
  "Local flag that indicates the current buffer was originally in VIRTUAL mode")

(defvar sr-dired-directory ""
  "Directory inside which sr-mode is currently active")

(defvar sr-start-message
  "Been coding all night? Enjoy the Sunrise! (or press q to quit)"
  "Message to display when `sr' is started.")

(defface sr-active-path-face
  '((t (:background "#ace6ac" :foreground "yellow" :bold t :height 120)))
  "Face of the directory path in the active pane"
  :group 'sunrise)

(defface sr-passive-path-face
  '((t (:background "white" :foreground "lightgray" :bold t :height 120)))
  "Face of the directory path in the passive pane"
  :group 'sunrise)

(defface sr-editing-path-face
  '((t (:background "red" :foreground "yellow" :bold t :height 120)))
  "Face of the directory path in the active pane while in editable pane mode"
  :group 'sunrise)

(defface sr-highlight-path-face
  '((t (:background "yellow" :foreground "#ace6ac" :bold t :height 120)))
  "Face of the directory path on mouse hover"
  :group 'sunrise)

(defface sr-clex-hotchar-face
  '((t (:foreground "red" :bold t)))
  "Face of the hot character (%) in CLEX mode. Indicates that a CLEX
substitution may be about to happen."
  :group 'sunrise)

;;; ============================================================================
;;; This is the core of Sunrise: the main idea is to apply sr-mode only inside
;;; Sunrise buffers while keeping all of dired-mode untouched.

(define-derived-mode sr-mode dired-mode "Sunrise Commander"
  "Two-pane file manager for Emacs based on Dired and inspired by MC. The
  following keybindings are available:

        /, j .......... go to directory
        p, n .......... move cursor up/down
        M-p, M-n ...... move cursor up/down in passive pane
        ^, J .......... go to parent directory
        M-^, M-J ...... go to parent directory in passive pane
        Tab ........... switch to other pane
        C-Tab.......... switch to viewer window
        C-c Tab ....... switch to viewer window (console compatible)
        RET, f ........ visit selected file/directory
        M-RET, M-f .... visit selected file/directory in passive pane
        C-c RET ....... visit selected in passive pane (console compatible)
        b ............. visit selected file/directory in default browser
        F ............. visit all marked files, each in its own window
        C-u F ......... visit all marked files in the background
        o,v ........... quick visit selected file (scroll with C-M-v, C-M-S-v)
        C-u o, C-u v .. kill quick-visited buffer (restores normal scrolling)

        + ............. create new directory
        C ............. copy marked (or current) files and directories
        R ............. rename marked (or current) files and directories
        S ............. soft-link selected file/directory to passive pane
        Y ............. do relative soft-link of selected file in passive pane
        H ............. hard-link selected file to passive pane
        M-C ........... copy (using traditional dired-do-copy)
        M-R ........... rename (using traditional dired-do-rename)
        M-S............ soft-link (using traditional dired-do-symlink)
        M-Y............ do relative soft-link (with traditional dired-do-relsymlink)
        M-H............ hard-link selected file/directory to passive pane
        D ............. delete marked (or current) files and directories
        A ............. search marked files for regular expression
        Q ............. perform query-replace-regexp on marked files

        M-a ........... move to beginning of current directory
        M-e ........... move to end of current directory
        M-y ........... go to previous directory in history
        M-u ........... go to next directory in history
        C-M-y ......... go to previous directory in history on passive pane
        C-M-u ......... go to next directory in history on passive pane

        g ............. refresh pane
        s ............. change sorting order or files (name/size/time/extension)
        C-o ........... show/hide hidden files (requires dired-omit-mode)
        C-Backspace ... hide/show file attributes in pane
        C-c Backspace . hide/show file attributes in pane (console compatible)
        M-l ........... truncate/continue long lines in pane
        C-c C-w ....... browse directory tree using w3m

        M-t ........... transpose panes
        M-o ........... synchronize panes
        C-c C-s ....... change panes layout (vertical/horizontal/top-only)
        [ ............. enlarges the right pane by 5 columns
        ] ............. enlarges the left pane by 5 columns
        C-c C-z ....... enable/disable synchronized navigation

        C-= ........... smart compare files (ediff)
        C-c = ......... smart compare files (console compatible)
        = ............. fast smart compare files (plain diff)
        C-M-= ......... compare directories
        C-x = ......... compare directories (console compatible)

        C-c C-f ....... execute find-dired in Sunrise VIRTUAL mode
        C-c C-n ....... execute find-name-dired in Sunrise VIRTUAL mode
        C-c C-g ....... execute find-grep-dired in Sunrise VIRTUAL mode
        C-c C-l ....... execute locate in Sunrise VIRTUAL mode
        C-c C-r ....... browse list of recently visited files (requires recentf)
        C-c C-c ....... [after find, locate or recent] dismiss virtual buffer
        ; ............. follow file (go to same directory as selected file)
        M-; ........... follow file in passive pane

        C-> ........... save named checkpoint (a.k.a. \"bookmark panes\")
        C-c > ......... save named checkpoint (console compatible)
        C-.    ........ restore named checkpoint
        C-c .  ........ restore named checkpoint

        C-x C-q ....... put pane in Editable Dired mode (commit with C-c C-c)
        @! ............ fast backup files (but not dirs!), each to [filename].bak

        C-c t ......... open new terminal or switch to already open one
        C-c T ......... open terminal AND change directory to current
        C-c C-t ....... open always a new terminal in current directory
        q ............. quit Sunrise Commander, restore previous window setup
        M-q ........... quit Sunrise Commander, don't restore previous windows

Additionally, if you activate the mc-compatible keybindings (by invoking the
sunrise-mc-keys function) you'll get the following ones:

        F2 ............ go to directory
        F3 ............ quick visit selected file
        F4 ............ visit selected file
        F5 ............ copy marked (or current) files and directories
        F6 ............ rename marked (or current) files and directories
        F7 ............ create new directory
        F8 ............ delete marked (or current) files and directories
        F10 ........... quit Sunrise Commander
        Insert ........ mark file
        C-PgUp ........ go to parent directory

Any other dired keybinding (not overridden by any of the above) can be used in
Sunrise, like G for changing group, M for changing mode and so on.

Some more bindings are provided for terminals in line mode, most useful after
opening a terminal in the viewer window (with C-c t):

       (these two are only for external shells - bash, ksh, etc. not for eshell)
        C-c C-j ....... put terminal in line mode
        C-c C-k ....... put terminal back in char mode

        M-<up>, M-P ... move cursor up in active pane
        M-<down>, M-N . move cursor down in active pane
        M-Return ...... visit selected file/directory in active pane
        M-J ........... go to parent directory in active pane
        M-M ........... mark selected file/directory in active pane
        M-Backspace ... unmark previous file/directory in active pane
        M-U ........... remove all marks from active pane
        C-Tab ......... switch focus to active pane

In a terminal in line mode the following substitutions are also performed
automatically:

       %f - expands to the currently selected file in the left pane
       %F - expands to the currently selected file in the right pane
       %m - expands to the list of paths of all marked files in the left pane
       %M - expands to the list of paths of all marked files in the right pane
       %n - expands to the list of names of all marked files in the left pane
       %N - expands to the list of names of all marked files in the right pane
       %d - expands to the current directory in the left pane
       %D - expands to the current directory in the right pane
       %% - inserts a single % sign.
"
  :group 'sunrise
  (set-keymap-parent sr-mode-map dired-mode-map)

  (make-local-variable 'dired-recursive-deletes)
  (setq dired-recursive-deletes 'top)

  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows (sr-truncate-v t))

  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil)
)

(define-derived-mode sr-virtual-mode dired-virtual-mode "Sunrise VIRTUAL"
  "Sunrise Commander Virtual Mode. Useful for reusing find and locate results."
  :group 'sunrise
  (set-keymap-parent sr-virtual-mode-map sr-mode-map)
  (sr-highlight)

  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows (sr-truncate-v t))

  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil)

  (define-key sr-virtual-mode-map "\C-c\C-c" 'sr-virtual-dismiss))

(defmacro sr-within (dir form)
  "Puts the given form in Sunrise context."
  `(progn
     (setq sr-dired-directory
           (file-name-as-directory (abbreviate-file-name dir)))
     (ad-activate 'dired-find-buffer-nocreate)
     ,form
     (ad-deactivate 'dired-find-buffer-nocreate)
     (setq sr-dired-directory "")))

(defmacro sr-save-aspect (&rest body)
  "Restores hidden attributes and omit mode after a directory transition."
  `(let ((hidden-attrs (get sr-selected-window 'hidden-attrs))
         (omit (or dired-omit-mode -1)))
     (hl-line-mode 0)
     ,@body
     (if hidden-attrs
         (sr-hide-attributes))
     (sr-omit-mode omit)))

(defmacro sr-alternate-buffer (form)
  "Executes form in a new buffer, after killing the previous one."
  `(let ((dispose nil))
     (unless (or (not dired-directory)
                 (equal sr-left-buffer sr-right-buffer))
       (setq dispose (current-buffer)))
     ,form
     (setq sr-this-directory default-directory)
     (sr-keep-buffer)
     (if dispose (kill-buffer dispose))))

(defun sr-dired-mode ()
  "Sets Sunrise mode in every Dired buffer opened in Sunrise (called in hook)."
  (if (and sr-running
           (sr-equal-dirs dired-directory default-directory)
           (not (equal major-mode 'sr-mode)))
      (let ((dired-listing-switches dired-listing-switches))
        (if (null (string-match "^/ftp:" default-directory))
            (setq dired-listing-switches sr-listing-switches))
        (sr-mode)
        (dired-unadvertise dired-directory))))
(add-hook 'dired-before-readin-hook 'sr-dired-mode)

(defun sr-virtual-dismiss ()
  "Restores normal view of pane in Sunrise VIRTUAL mode."
  (interactive)
  (if (equal major-mode 'sr-virtual-mode)
      (sr-save-aspect
       (sr-alternate-buffer (sr-goto-dir sr-this-directory))
       (sr-revert-buffer))))

;; This is a hack to avoid some dired mode quirks:
(defadvice dired-find-buffer-nocreate
  (before sr-advice-findbuffer (dirname &optional mode))
  (if (sr-equal-dirs sr-dired-directory dirname)
      (setq mode 'sr-mode)))

;; Handles panes opened from bookmarks in Sunrise:
(defadvice bookmark-jump
  (around sr-advice-bookmark-jump (str))
  (if (memq major-mode '(sr-mode sr-virtual-mode))
      (let ((target (bookmark-get-filename str))
            (dispose nil))
        (if (not (eq sr-left-buffer sr-right-buffer))
            (setq dispose (current-buffer)))
        (if (sr-equal-dirs target sr-other-directory)
            (sr-synchronize-panes t)
          (let ((sr-dired-directory target))
            (sr-save-aspect ad-do-it)))
        (if dispose (kill-buffer dispose))
        (sr-keep-buffer))
    ad-do-it)
  (setq sr-this-directory default-directory))
(ad-activate 'bookmark-jump)

;; Tweaks the target directory guessing mechanism:
(defadvice dired-dwim-target-directory
  (around sr-advice-dwim-target ())
  (if sr-running
      (setq ad-return-value sr-other-directory)
    ad-do-it))
(ad-activate 'dired-dwim-target-directory)

;; Fixes dired-goto-file and all functions that depend on it in *nix systems
;; in which directory names end with a slash.
(defadvice dired-get-filename
  (around sr-advice-dired-get-filename (&optional localp no-error-if-not-filep))
  ad-do-it
  (if ad-return-value
      (setq ad-return-value
            (replace-regexp-in-string "/$" "" ad-return-value))))
(ad-activate 'dired-get-filename)

;; inhibits reverting sunrise virtual buffers:
(defadvice revert-buffer
  (around sr-advice-revert-buffer ())
  (unless (or (equal major-mode 'sr-virtual-mode)
              (local-variable-p 'sr-virtual-buffer))
    ad-do-it))
(ad-activate 'revert-buffer)

;;; ============================================================================
;;; Sunrise Commander keybindings:

(define-key sr-mode-map "\C-m"                'sr-advertised-find-file)
(define-key sr-mode-map "f"                   'sr-advertised-find-file)
(define-key sr-mode-map "o"                   'sr-quick-view)
(define-key sr-mode-map "v"                   'sr-quick-view)
(define-key sr-mode-map "/"                   'sr-goto-dir)
(define-key sr-mode-map "j"                   'sr-goto-dir)
(define-key sr-mode-map "^"                   'sr-dired-prev-subdir)
(define-key sr-mode-map "J"                   'sr-dired-prev-subdir)
(define-key sr-mode-map "\M-y"                'sr-history-prev)
(define-key sr-mode-map "\M-u"                'sr-history-next)
(define-key sr-mode-map "\C-c>"               'sr-checkpoint-save)
(define-key sr-mode-map "\C-c."               'sr-checkpoint-restore)
(define-key sr-mode-map "\t"                  'sr-change-window)
(define-key sr-mode-map "\C-c\t"              'sr-select-viewer-window)
(define-key sr-mode-map "\M-a"                'sr-beginning-of-buffer)
(define-key sr-mode-map "\M-e"                'sr-end-of-buffer)
(define-key sr-mode-map "\C-c\C-s"            'sr-split-toggle)
(define-key sr-mode-map "\M-t"                'sr-transpose-panes)
(define-key sr-mode-map "]"                   'sr-enlarge-left-pane)
(define-key sr-mode-map "["                   'sr-enlarge-right-pane)
(define-key sr-mode-map "\M-o"                'sr-synchronize-panes)
(define-key sr-mode-map "\C-o"                'sr-omit-mode)
(define-key sr-mode-map "b"                   'sr-browse-file)
(define-key sr-mode-map "\C-c\C-w"            'sr-browse-pane)
(define-key sr-mode-map "g"                   'sr-revert-buffer)
(define-key sr-mode-map "\C-c\d"              'sr-toggle-attributes)
(define-key sr-mode-map "\M-l"                'sr-toggle-truncate-lines)
(define-key sr-mode-map "s"                   'sr-interactive-sort)
(define-key sr-mode-map "\C-c\C-z"            'sr-sync)

(define-key sr-mode-map "C"                   'sr-do-copy)
(define-key sr-mode-map "R"                   'sr-do-rename)
(define-key sr-mode-map "S"                   'sr-do-symlink)
(define-key sr-mode-map "Y"                   'sr-do-relsymlink)
(define-key sr-mode-map "H"                   'sr-do-hardlink)
(define-key sr-mode-map "\M-C"                'dired-do-copy)
(define-key sr-mode-map "\M-R"                'dired-do-rename)
(define-key sr-mode-map "\M-S"                'dired-do-symlink)
(define-key sr-mode-map "\M-Y"                'dired-do-relsymlink)
(define-key sr-mode-map "\M-H"                'dired-do-hardlink)
(define-key sr-mode-map "\C-x\C-q"            'sr-editable-pane)
(define-key sr-mode-map "@"                   'sr-fast-backup-files)

(define-key sr-mode-map "="                   'sr-diff)
(define-key sr-mode-map "\C-c="               'sr-ediff)
(define-key sr-mode-map "\C-x="               'sr-compare-dirs)

(define-key sr-mode-map "\C-c\C-f"            'sr-find)
(define-key sr-mode-map "\C-c\C-n"            'sr-find-name)
(define-key sr-mode-map "\C-c\C-g"            'sr-find-grep)
(define-key sr-mode-map "\C-c\C-l"            'sr-locate)
(define-key sr-mode-map "\C-c\C-r"            'sr-recent-files)
(define-key sr-mode-map "\C-c\C-d"            'sr-recent-directories)
(define-key sr-mode-map "\C-c\C-v"            'sr-pure-virtual)
(define-key sr-mode-map ";"                   'sr-follow-file)
(define-key sr-mode-map "Q"                   'sr-do-query-replace-regexp)
(define-key sr-mode-map "F"                   'sr-do-find-marked-files)
(define-key sr-mode-map "A"                   'sr-do-search)
(define-key sr-mode-map "\C-x\C-f"            'sr-find-file)

(define-key sr-mode-map "\M-n"                'sr-next-line-other)
(define-key sr-mode-map [M-down]              'sr-next-line-other)
(define-key sr-mode-map [A-down]              'sr-next-line-other)
(define-key sr-mode-map "\M-p"                'sr-prev-line-other)
(define-key sr-mode-map [M-up]                'sr-prev-line-other)
(define-key sr-mode-map [A-up]                'sr-prev-line-other)
(define-key sr-mode-map "\M-\C-m"             'sr-advertised-find-file-other)
(define-key sr-mode-map "\M-f"                'sr-advertised-find-file-other)
(define-key sr-mode-map "\C-c\C-m"            'sr-advertised-find-file-other)
(define-key sr-mode-map "\M-^"                'sr-prev-subdir-other)
(define-key sr-mode-map "\M-J"                'sr-prev-subdir-other)
(define-key sr-mode-map "\M-U"                'sr-unmark-all-marks-other)
(define-key sr-mode-map "\M-;"                'sr-follow-file-other)
(define-key sr-mode-map "\C-\M-y"             'sr-history-prev-other)
(define-key sr-mode-map "\C-\M-u"             'sr-history-next-other)

(define-key sr-mode-map "\C-ct"               'sr-term)
(define-key sr-mode-map "\C-cT"               'sr-term-cd)
(define-key sr-mode-map "\C-c\C-t"            'sr-term-cd-newterm)
(define-key sr-mode-map "q"                   'sr-quit)
(define-key sr-mode-map "\M-q"                'sunrise-cd)
(define-key sr-mode-map "h"                   'sr-describe-mode)
(define-key sr-mode-map "?"                   'sr-summary)

(define-key sr-mode-map [mouse-2]             (lambda ()
                                                (interactive)
                                                (call-interactively 'mouse-set-point)
                                                (sr-advertised-find-file)))
(define-key sr-mode-map [follow-link]         'mouse-face)

(if window-system
    (progn
      (define-key sr-mode-map [(control >)]         'sr-checkpoint-save)
      (define-key sr-mode-map [(control .)]         'sr-checkpoint-restore)
      (define-key sr-mode-map [(control tab)]       'sr-select-viewer-window)
      (define-key sr-mode-map [(control backspace)] 'sr-toggle-attributes)
      (define-key sr-mode-map [(control ?\=)]       'sr-ediff)
      (define-key sr-mode-map [(control meta ?\=)]  'sr-compare-dirs)))

(defun sunrise-mc-keys ()
  "Binds the function keys F2 to F10 the traditional MC way."
  (interactive)
  (define-key sr-mode-map [(f2)]            'sr-goto-dir)
  (define-key sr-mode-map [(f3)]            'sr-quick-view)
  (define-key sr-mode-map [(f4)]            'sr-advertised-find-file)
  (define-key sr-mode-map [(f5)]            'sr-do-copy)
  (define-key sr-mode-map [(f6)]            'sr-do-rename)
  (define-key sr-mode-map [(f7)]            'dired-create-directory)
  (define-key sr-mode-map [(f8)]            'dired-do-delete)
  (define-key sr-mode-map [(f10)]           'keyboard-escape-quit)
  (define-key sr-mode-map [(insert)]        'dired-mark)
  (define-key sr-mode-map [(control prior)] 'sr-dired-prev-subdir))

;;; ============================================================================
;;; Initialization and finalization functions:

(defun sunrise (&optional left-directory right-directory filename)
  "Starts the Sunrise Commander. If the param `left-directory' is given the left
  window  will  display  this  directory  (the  same   for   `right-directory').
  Specifying nil for any of these values uses the default, ie. home."
  (interactive)
  (message "Starting Sunrise Commander...")

  (if (not sr-running)
      (let ((welcome sr-start-message))
        (if left-directory
            (setq sr-left-directory left-directory))
        (if right-directory
            (setq sr-right-directory right-directory))
        
        (setq sr-running t)
        (setq sr-restore-buffer (current-buffer))
        (setq sr-prior-window-configuration (current-window-configuration))
        (sr-setup-windows)
        (if filename
            (condition-case description
                (sr-focus-filename (replace-regexp-in-string ".*/" "" filename))
              (error (setq welcome (second description)))))
        (setq sr-this-directory default-directory)
        (setq sr-current-frame (window-frame (selected-window)))
        (message "%s" welcome)
        (sr-highlight)) ;;<-- W32Emacs needs this 
    (let ((my-frame (window-frame (selected-window))))
      (sr-quit)
      (message "All life leaps out to greet the light...")
      (unless (eq my-frame (window-frame (selected-window)))
        (select-frame my-frame)
        (sunrise left-directory right-directory filename)))))

(eval-and-compile
  (defun sr-symbol (side context)
    "Synthesizes Sunrise symbols (sr-left-buffer, sr-right-window, etc.)"
    (intern (concat "sr-" (symbol-name side) "-" (symbol-name context)))))

(defun sunrise-cd ()
  "Run Sunrise but give it the current directory to use."
  (interactive)
  (if (not sr-running)
      (let((to-focus (buffer-file-name))
           (to-kill (symbol-value (sr-symbol sr-selected-window 'buffer))))
        (if (buffer-live-p to-kill) (kill-buffer to-kill))
        (if (equal sr-selected-window 'left)
            (sunrise default-directory nil to-focus)
          (sunrise nil default-directory to-focus)))
    (progn
      (sr-quit t)
      (message "Hast thou a charm to stay the morning-star in his deep course?"))))

(defun sr-dired (directory)
  "Visits the given directory (or file) in sr-mode."
  (interactive
   (list
    (read-file-name "Change directory (file or pattern): " nil nil nil)))
  (if (and (file-exists-p directory) (file-readable-p directory))
      (if (file-directory-p directory)
          (let ((dired-omit-mode (if sr-show-hidden-files -1 1)))
            (sr-goto-dir directory)
            (unless sr-show-file-attributes
              (sr-hide-attributes)))
        (sr-quit))))

;;; ============================================================================
;;; Window management functions:

(defmacro sr-setup-pane (side)
  "Helper macro for function sr-setup-windows."
  `(let ((sr-selected-window ',side))
     (setq ,(sr-symbol side 'window) (selected-window))
     (if (buffer-live-p ,(sr-symbol side 'buffer))
         (progn
           (switch-to-buffer ,(sr-symbol side 'buffer))
           (setq ,(sr-symbol side 'directory) default-directory))
       (sr-dired ,(sr-symbol side 'directory)))))

(defun sr-setup-windows()
  "Setup the Sunrise window configuration (two windows in sr-mode.)"

  ;;get rid of all windows except one (not any of the panes!)
  (sr-select-viewer-window)
  (delete-other-windows)

  ;;now create the viewer window
  (split-window (selected-window) (* 2 (/ (window-height) 3)))

  (cond
   ((equal sr-window-split-style 'horizontal) (split-window-horizontally))
   ((equal sr-window-split-style 'vertical)   (split-window-vertically))
   ((equal sr-window-split-style 'top)        (split-window-vertically))
   (t (error "ERROR: Don't know how to split this window: %s" sr-window-split-style)))

  ;;setup sunrise on both panes
  (sr-setup-pane left)
  (other-window 1)
  (sr-setup-pane right)

  ;;select the correct window
  (sr-select-window sr-selected-window)

  (if (equal sr-window-split-style 'top)
      (delete-window sr-right-window)
    (sr-force-passive-highlight))
  (run-hooks 'sr-start-hook))

(defun sr-lock-window (frame)
  "Resize the left Sunrise pane to have the \"right\" size."
  (if (and sr-running
           sr-windows-locked
           (not sr-ediff-on)
           (window-live-p sr-left-window))
      (save-selected-window
        (select-window sr-left-window)
        (let* ((my-style-factor (if (equal sr-window-split-style 'horizontal) 2 1))
               (my-pane-height (* my-style-factor (/ (frame-height) 3)))
               (my-delta (- my-pane-height (window-height))))
          (enlarge-window my-delta)))))

;; This keeps the size of the Sunrise panes constant:
(add-hook 'window-size-change-functions 'sr-lock-window)

(defun sr-select-window (side)
  "Select/highlight the given sr window (right or left)."
  (select-window (symbol-value (sr-symbol side 'window)))
  (setq sr-selected-window side)
  (sr-highlight))

(defun sr-select-viewer-window ()
  "Tries to select a window that is not a sr pane."
  (interactive)
  (dotimes (times 2)
    (if (memq (selected-window) (list sr-left-window sr-right-window))
        (other-window 1))))

(defun sr-highlight(&optional face)
  "Sets up the path line in the current buffer."
  (when (memq major-mode '(sr-mode sr-virtual-mode))
    (save-excursion
      (goto-char (point-min))
      (sr-hide-avfs-root)
      (if window-system
          (progn
            (sr-graphical-highlight face)
            (sr-force-passive-highlight))))
    (hl-line-mode 1)))

(defun sr-graphical-highlight (&optional face)
  "Sets up the graphical path line in the current buffer (fancy fonts and
  clickable path)."
  (let ((my-face (or face 'sr-active-path-face))
        (begin) (end))
    ;;determine begining and end
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "\\S " nil t)
      (setq begin (1- (point)))
      (end-of-line)
      (setq end (1- (point))))

    ;;setup overlay
    (setq sr-current-window-overlay (make-overlay begin end))
    (overlay-put sr-current-window-overlay 'face my-face)
    (overlay-put sr-current-window-overlay 'window (selected-window))

    ;;make path line clickable
    (toggle-read-only -1)
    (add-text-properties
     begin
     end
     '(mouse-face sr-highlight-path-face
                  help-echo "mouse-2: move up")
     nil)
    (toggle-read-only 1)))

(defun sr-force-passive-highlight (&optional revert)
  "Sets  up  the graphical path line in the passive pane. With optional argument
  'revert' executes sr-revert-buffer on the passive buffer."
  (if (and (window-live-p sr-left-window) (window-live-p sr-right-window))
      (let ((my-window (if (equal sr-selected-window 'left)
                           sr-right-window
                         sr-left-window)))
        (save-window-excursion
          (select-window my-window)
          (if revert (sr-revert-buffer))
          (sr-graphical-highlight 'sr-passive-path-face)
          (hl-line-mode 0)))))

(defun sr-hide-avfs-root ()
  "Hides the AVFS virtual filesystem root (if any) on the path line."
  (if sr-avfs-root
      (let ((next (search-forward (concat sr-avfs-root "/") nil t))
            (len (length sr-avfs-root))
            (overlay))
        (while next
          (progn
            (setq overlay (make-overlay (- next len) next))
            (overlay-put overlay 'invisible t)
            (overlay-put overlay 'intangible t)
            (setq next (search-forward sr-avfs-root nil t))))
        (goto-char (point-min)))))

(defun sr-quit (&optional norestore)
  "Quit Sunrise and restore emacs to previous operation."
  (interactive)
  (if sr-running
      (progn
        (setq sr-running nil)
        (sr-save-directories)
        (if norestore
            (progn
              (sr-select-viewer-window)
              (delete-other-windows))
          (progn
            ;;restore previous window setup
            (set-window-configuration sr-prior-window-configuration)
            (if (buffer-live-p sr-restore-buffer)
                (set-buffer sr-restore-buffer))))
        (sr-bury-panes)
        (toggle-read-only -1)
        (run-hooks 'sr-quit-hook)
        (setq sr-current-frame nil))
    (bury-buffer)))

(add-hook 'delete-frame-functions
          '(lambda (frame)
             (if (and sr-running (eq frame sr-current-frame)) (sr-quit))))

(defun sr-save-directories ()
  "Save the current directories in the panes to use the next time sr starts up."
  (when (window-live-p sr-left-window)
    (set-buffer (window-buffer sr-left-window))
    (when (equal major-mode 'sr-mode)
      (setq sr-left-directory default-directory)
      (setq sr-left-buffer (current-buffer))))

  (when (window-live-p sr-right-window)
    (set-buffer (window-buffer sr-right-window))
    (when (equal major-mode 'sr-mode)
      (setq sr-right-directory default-directory)
      (setq sr-right-buffer (current-buffer)))))

(defun sr-bury-panes ()
  "Sends both pane buffers to the end of the emacs list of buffers."
  (bury-buffer (buffer-name sr-left-buffer))
  (bury-buffer (buffer-name sr-right-buffer)))

(defun sr-resize-panes (&optional reverse)
  "Enlarges (or shrinks, if reverse is t) the left pane by 5 columns."
  (if (and (window-live-p sr-left-window)
           (window-live-p sr-right-window))
      (let ((direction (or (and reverse -1) 1)))
        (save-selected-window
          (select-window sr-left-window)
          (enlarge-window-horizontally (* 5 direction))))))

(defun sr-enlarge-left-pane ()
  "Enlarges the left pane by 5 columns."
  (interactive)
  (sr-resize-panes))

(defun sr-enlarge-right-pane ()
  "Enlarges the right pane by 5 columns."
  (interactive)
  (sr-resize-panes t))

;;; ============================================================================
;;; File system navigation functions:

(defun sr-advertised-find-file (&optional filename)
  "Manages the two basic cases of file name: directories are open inside Sunrise
  itself, while regular files are passed to  sr-find-file.  A  special  case  of
  directory is when the user presses return, f, or clicks on the path line."
  (interactive)
  (save-excursion
    (if (null filename)
        (if (eq 1 (line-number-at-pos)) ;; <- Click or Enter on path line
            (let* ((eol (save-excursion (end-of-line) (point)))
                   (slash (re-search-forward "/" eol t)))
              (if slash
                  (setq filename (buffer-substring (+ 2 (point-min)) slash))
                (setq filename default-directory)))
          (setq filename (expand-file-name (dired-get-filename nil t)))))
    (if filename
        (if (file-directory-p filename)
            (progn
              (setq filename (file-name-as-directory filename))
              (if (string= filename (expand-file-name "../"))
                  (sr-dired-prev-subdir)
                (sr-goto-dir filename)))
          (sr-find-file filename)))))

(defun sr-find-file (filename &optional wildcards)
  "Determines  the  proper  way  of handling a file. If the file is a compressed
  archive and AVFS has been activated, first tries to display it as a  directory
  in the VFS, otherwise just visits the file."
  (interactive (find-file-read-args "Find file: " nil))
  (let ((mode (assoc-default filename auto-mode-alist 'string-match)))
    (when (and sr-avfs-root
	       (or (eq 'archive-mode mode)
		   (eq 'tar-mode mode)
		   (and (listp mode) (eq 'jka-compr (second mode)))
		   (eq 'avfs-mode mode)))
      (let ((vfile (sr-avfs-dir filename)))
	(when vfile
	  (sr-goto-dir vfile)
	  (setq filename nil))))
    (when (eq 'sr-virtual-mode mode)
      (sr-save-aspect
       (sr-alternate-buffer (find-file filename)))
      (sr-history-push filename)
      (set-visited-file-name nil t)
      (setq filename nil)))

  (if (null filename) ;;the file is a virtual directory:
      (sr-keep-buffer)
    (progn ;;the file is a regular file:
      (condition-case description
          (progn
            (find-file filename wildcards)
            (delete-other-windows)
            (setq sr-prior-window-configuration (current-window-configuration))
            (sr-quit))
        (error (message "%s" (second description)))) )))

(defun sr-avfs-dir (filename)
  "Returns the virtual path for accessing the given file through AVFS, or nil if
   AVFS cannot manage this kind of file."
  (let* ((handler (assoc-default filename sr-avfs-handlers-alist 'string-match))
          (vdir (concat sr-avfs-root filename handler)))
     (if (file-directory-p vdir) vdir nil)))

(defun sr-goto-dir (dir)
  "Changes the current directory in the active pane to the given one."
  (interactive "DChange directory (file or pattern): ")
  (unless (and (eq major-mode 'sr-mode) (sr-equal-dirs dir default-directory))
    (if (and sr-avfs-root
             (null (posix-string-match "#" dir)))
        (setq dir (replace-regexp-in-string sr-avfs-root "" dir)))
    
    ;; Detect spontaneous windows changes (using the mouse):
    (when (and (not (sr-equal-dirs sr-this-directory default-directory))
               (sr-equal-dirs sr-other-directory default-directory)
               (not (local-variable-p 'sr-virtual-buffer)))
      (setq sr-other-directory sr-this-directory)
      (sr-force-passive-highlight))
    (if (eq (selected-window) sr-left-window)
        (sr-select-window 'left)
      (sr-select-window 'right))
    
    (sr-save-aspect
     (sr-within dir (sr-alternate-buffer (dired dir))))
    (sr-history-push default-directory)
    (sr-beginning-of-buffer)))

(defun sr-dired-prev-subdir ()
  "Go to the previous subdirectory."
  (interactive)
  (if (not (string= default-directory "/"))
      (let ((here (sr-directory-name-proper (expand-file-name default-directory))))
        (setq here (replace-regexp-in-string "#.*/?$" "" here))
        (sr-goto-dir (expand-file-name "../"))
        (sr-focus-filename here))
    (error "ERROR: Already at root")))

(defun sr-follow-file (&optional target-path)
  "Go to the same directory where the selected file is. Very useful inside
   Sunrise VIRTUAL buffers."
  (interactive)
  (if (null target-path)
      (setq target-path (dired-get-filename nil t)))

  (let ((target-dir (file-name-directory target-path))
        (target-symlink (file-symlink-p target-path))
        (target-file))

    ;; if the target is a symlink and there's nothing more interesting to do
    ;; then follow the symlink:
    (when (and target-symlink
               (string= target-dir (dired-current-directory))
               (not (eq major-mode 'sr-virtual-mode)))
      (setq target-path target-symlink)
      (setq target-dir (file-name-directory target-symlink)))

    (setq target-file (file-name-nondirectory target-path))

    (if target-dir ;; <-- nil in symlinks to other files in same directory:
        (sr-goto-dir target-dir))
    (sr-focus-filename target-file)))

(defun sr-history-push (element)
  "Pushes a new path into the history ring of the current pane."
  (unless (null element)
    (let* ((pane (assoc sr-selected-window sr-history-registry))
           (hist (cdr pane))
           (len (length hist)))
      (if (>= len sr-history-length)
          (nbutlast hist (- len sr-history-length)))
      (if (< 1 (length element))
          (setq element (replace-regexp-in-string "/?$" "" element)))
      (setq hist (delete element hist))
      (push element hist)
      (setcdr pane hist))))

(defun sr-history-next ()
  "Changes the current directory to the next one (if any) in the history list of
  the current pane."
  (interactive)
  (sr-history-move 'sr-history-unwind))

(defun sr-history-prev ()
  "Changes  the  current  directory  to the previous one (if any) in the history
  list of the current pane."
  (interactive)
  (sr-history-move 'sr-history-wind))

(defun sr-history-move (fun)
  "Moves  the current pane backwards and forwards through its history of visited
  directories, depending on the given direction function (wind or unwind)."
  (let* ((pane (assoc sr-selected-window sr-history-registry))
         (hist (cdr pane))
         (hist (apply fun (list hist)))
         (item (car hist)))
    (if item
        (progn
          (setcdr pane hist)
          (cond ((file-directory-p item) (sr-goto-dir item))
                ((file-exists-p item) (sr-find-file item))
                (t (ignore)))
          ))))

(defmacro sr-pick-file (item hist pick-next)
  "Helper macro for implementing sr-history-wind and sr-history-unwind. Executes
  pick-next until item becomes a valid file or hist runs out of elements."
  `(while (and (> (length ,hist) 0)
               (or (null ,item) (not (file-exists-p ,item))))
     ,pick-next))

(defun sr-history-wind (hist)
  "Rotates clockwise the elements in the given history ring, ie. takes the first
  element and puts it at the end of the list. Additionally discards all elements
  that did not represent valid files when the function was executed."
  (let ((item) (head))
    (sr-pick-file item hist (setq item (pop hist)))
    (setq head (car hist))
    (sr-pick-file head hist (progn (pop hist) (setq head (car hist))))
    (if item
        (append hist (list item))
      hist)))

(defun sr-history-unwind (hist)
  "Rotates  counter-clockwise  the  elements inthe given history ring, ie. takes
  the last element and puts it  at  the  beginning  of  the  list.  Additionally
  discards all elements that did not represent valid files when the function was
  executed. (WARNING: uses nbutlast, destroys its own input list)."
  (let (item)
    (sr-pick-file item hist (progn
                              (setq item (car (last hist)))
                              (setq hist (nbutlast hist))))
    (if item
        (cons item hist)
      hist)))

(defun sr-checkpoint-save (name)
  "Allows to give a name to the current directories in the Sunrise panes, so
  they can be restored later."
  (interactive "sCheckpoint name to save? ")
  (let ((my-cell (assoc-string name sr-checkpoint-registry)))
    (sr-save-directories)
    (if (null my-cell)
        (setq sr-checkpoint-registry
              (acons name
                     (list sr-left-directory sr-right-directory)
                     sr-checkpoint-registry))
      (setcdr my-cell (list sr-left-directory sr-right-directory)))
  (message "%s" (concat "Checkpoint \"" name "\" saved"))))

(defun sr-checkpoint-restore (name)
  "Allows to restore a previously saved checkpoint."
  (interactive "sCheckpoint name to restore? " )
  (let* ((cp-list (assoc-string name sr-checkpoint-registry))
         (dirs-list (cdr cp-list)))
    (unless cp-list
      (error (concat "No such checkpoint: " name)))
    (if (equal sr-selected-window 'right)
        (setq dirs-list (reverse dirs-list)))
    (mapc '(lambda (x) (sr-goto-dir x) (sr-change-window)) dirs-list)))

(defun sr-do-find-marked-files (&optional noselect)
  "Sunrise replacement for dired-do-marked-files."
  (interactive "P")
  (unwind-protect
      (let ((files (dired-get-marked-files)))
        (unless noselect (sr-quit))
        (dired-simultaneous-find-file files noselect))
    (unless noselect (sr-quit))))

;;; ============================================================================
;;; graphical interface interaction functions:

(defun sr-change-window()
  "Change to the other sr buffer."
  (interactive)
  (if (and (window-live-p sr-left-window) (window-live-p sr-right-window))
      (let ((here sr-this-directory))
        (setq sr-this-directory sr-other-directory)
        (setq sr-other-directory here)
        (if (equal (selected-window) sr-right-window)
            (sr-select-window 'left)
          (sr-select-window 'right)))))

(defun sr-beginning-of-buffer()
  "Go to the first directory/file in dired."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward directory-listing-before-filename-regexp nil t)
      (while (looking-at "\.\.?/?$")
        (dired-next-line 1))))

(defun sr-end-of-buffer()
  "Go to the last directory/file in dired."
  (interactive)
  (goto-char (point-max))
  (re-search-backward directory-listing-before-filename-regexp)
  (dired-next-line 0))

(defun sr-focus-filename (filename)
  "Tries to select the given file name in the current buffer."
  (when (and dired-omit-mode
             (string-match (dired-omit-regexp) filename))
    (sr-omit-mode -1))
  (let ((expr filename))
    (when (or (file-directory-p filename) (file-symlink-p filename))
      (setq expr (replace-regexp-in-string "/$" "" expr))
      (setq expr (concat (regexp-quote expr) "\\(?:/\\| ->\\|$\\)")))
    (setq expr (concat "[0-9] +" expr))
    (beginning-of-line)
    (if (null (re-search-forward expr nil t))
        (if (null (re-search-backward expr nil t))
            (error (concat "ERROR: unable to find " filename
                           " in current directory")))))
  (beginning-of-line)
  (re-search-forward directory-listing-before-filename-regexp nil t))

(defun sr-split-toggle()
  "Changes sunrise windows layout from horizontal to vertical to top and so on."
  (interactive)
  (cond
   ((equal sr-window-split-style 'horizontal) (sr-split-setup 'vertical))
   ((equal sr-window-split-style 'vertical)   (sr-split-setup 'top))
   ((equal sr-window-split-style 'top)        (sr-split-setup 'horizontal))
   (t                                         (sr-split-setup 'horizontal))))

(defun sr-split-setup(split-type)
  (setq sr-window-split-style split-type)
  (when sr-running
    (if (equal sr-window-split-style 'top)
        (progn
          (sr-select-window 'left)
          (delete-window sr-right-window))
      (sr-setup-windows))
    (message "Sunrise: Split style changed to \"%s\"" (symbol-name split-type))))

(defun sr-transpose-panes ()
  "Changes the order of the panes."
  (interactive)
  (unless (sr-equal-dirs sr-this-directory sr-other-directory)
    (let ((dirs (list sr-other-directory sr-this-directory)))
      (mapc '(lambda (x) (sr-dired x) (sr-change-window)) dirs))))

(defun sr-synchronize-panes (&optional reverse)
  "Changes  the  directory  in the other pane to that in the current one. If the
  optional parameter reverse is set to t, performs the opposite operation,  i.e.
  changes the directory in the current pane to that in the other one."
  (interactive)
  (let ((target (current-buffer)))
    (sr-change-window)
    (if reverse
        (setq target (current-buffer))
      (sr-alternate-buffer (switch-to-buffer target)))
    (sr-change-window)
    (if reverse
        (sr-alternate-buffer (switch-to-buffer target)))))

(defun sr-browse-pane ()
  "Browses the directory in the active pane."
  (interactive)
  (if (not (featurep 'browse-url))
      (error "ERROR: Feature browse-url not available!")
    (let ((url (concat "file://" (expand-file-name default-directory))))
      (message "Browsing directory %s " default-directory)
      (if (featurep 'w3m)
          (eval '(w3m-goto-url url))
        (browse-url url)))))

(defun sr-browse-file (&optional file)
  "Displays the selected file in the default web browser."
  (interactive)
  (unless (featurep 'browse-url)
    (error "ERROR: Feature browse-url not available!"))
  (setq file (or file (dired-get-filename)))
  (sr-quit)
  (browse-url (concat "file://" file))
  (message "Browsing \"%s\" in web browser" file))

(defun sr-revert-buffer ()
  "Refreshes the current pane."
  (interactive)
  (sr-save-aspect (revert-buffer)))

(defun sr-omit-mode (&optional force)
  "Toggles dired-omit-mode."
  (interactive)
  (let ((hidden-attrs (get sr-selected-window 'hidden-attrs)))
    (dired-omit-mode force)
    (if hidden-attrs (sr-hide-attributes))
    (sr-highlight)))

(defun sr-quick-view (&optional arg)
  "Opens  the  selected file on the viewer window without selecting it. Kills
  any other buffer opened previously the same  way.  With  optional  argument
  kills the last quick view buffer without opening a new one."
  (interactive "P")
  (if arg
      (sr-kill-quick-view)
    (let ((split-width-threshold (* 10 (window-width))))
      (if (buffer-live-p other-window-scroll-buffer)
          (kill-buffer other-window-scroll-buffer))
      (save-selected-window
        (condition-case description
            (progn
              (dired-find-file-other-window)
              (sr-scrollable-viewer (current-buffer)))
          (error (message "%s" (second description)))))) ))

(defun sr-kill-quick-view ()
  "Kills the last buffer opened using quick view (if any)."
  (let ((buf other-window-scroll-buffer))
    (if (and (buffer-live-p buf)
             (y-or-n-p (concat "Kill buffer " (buffer-name buf) " ? ")))
        (kill-buffer buf))))

;; These clean up after a quick view:
(add-hook 'sr-quit-hook (lambda () (setq other-window-scroll-buffer nil)))
(add-hook 'kill-buffer-hook
          (lambda ()
            (if (eq (current-buffer) other-window-scroll-buffer)
                (setq other-window-scroll-buffer  nil))))

(defun sr-hide-attributes ()
  "Hides the attributes of all files in the active pane."
  (save-excursion
    (sr-unhide-attributes)
    (goto-char (point-min))
    (let ((next (re-search-forward directory-listing-before-filename-regexp nil t))
          (attr-list nil)
          (overlay nil))
      (while next
        (beginning-of-line)
        (setq overlay (make-overlay (+ 2 (point)) (- next 1)))
        (setq attr-list (cons overlay attr-list))
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'intangible t)
        (forward-line)
        (setq next (re-search-forward directory-listing-before-filename-regexp nil t)))
      (put sr-selected-window 'hidden-attrs attr-list))))

(defun sr-unhide-attributes ()
  "Shows the (hidden) attributes of all files in the active pane."
  (let ((attr-list (get sr-selected-window 'hidden-attrs)))
    (if attr-list
        (progn
          (mapc 'delete-overlay attr-list)
          (put sr-selected-window 'hidden-attrs nil)))))
;; (add-hook 'dired-after-readin-hook 'sr-unhide-attributes)

(defun sr-toggle-attributes ()
  "Hides/Shows the attributes of all files in the active pane."
  (interactive)
  (if (null (get sr-selected-window 'hidden-attrs))
      (progn
        (sr-hide-attributes)
        (message (concat "Sunrise: hiding attributes in " (symbol-name sr-selected-window) " pane")))
    (progn
      (sr-unhide-attributes)
      (message (concat "Sunrise: displaying attributes in " (symbol-name sr-selected-window) " pane")))))

(defun sr-toggle-truncate-lines ()
  "Enables/Disables truncation of long lines in the active pane."
  (interactive)
  (if (sr-truncate-p)
      (progn
        (setq truncate-partial-width-windows (sr-truncate-v nil))
        (message "Sunrise: continuing long lines"))
    (progn
      (setq truncate-partial-width-windows (sr-truncate-v t))
      (message "Sunrise: truncating long lines"))))

(defun sr-truncate-p nil
  "Returns  whether  truncate-partial-width-widows  is  set to truncate the long
  lines in the current pane. Used by sr-toggle-truncate-lines."
  (if (equal "23.0.60" (substring emacs-version 0 -2))
      (< 0 truncate-partial-width-windows)
    truncate-partial-width-windows))

(defun sr-truncate-v (active)
  "Returns the right value to set for truncate-partial-width-widows depending on
  the emacs version being used. Used by sr-toggle-truncate-lines."
  (or (and (equal "23.0.60" (substring emacs-version 0 -2))
           (or (and active 3000) 0))
      active))

(defun sr-interactive-sort (order)
  "Prompts for a new sorting order for the active pane and applies it."
  (interactive "cSort by (n)ame, (s)ize, (t)ime or e(x)tension? ")
  (if (>= order 97)
      (setq order (- order 32)))
  (cond ((eq order ?T) (sr-sort-order "TIME"      "t"))
        ((eq order ?S) (sr-sort-order "SIZE"      "S"))
        ((eq order ?X) (sr-sort-order "EXTENSION" "X"))
        (t             (sr-sort-order "NAME"      "" ))))

(defun sr-sort-order (label option)
  "Changes the sorting order of the active pane by appending additional options
   to dired-listing-switches and reverting the buffer."
  (if (equal major-mode 'sr-virtual-mode)
      (sr-sort-virtual option)
    (progn
      (put sr-selected-window 'sorting-order label)
      (let ((dired-listing-switches dired-listing-switches))
        (unless (string-match "^/ftp:" default-directory)
          (setq dired-listing-switches sr-listing-switches))
        (dired-sort-other (concat dired-listing-switches option) t))
      (sr-revert-buffer)))
  (message "%s" (concat "Sunrise: sorting entries by " label)))

(defun sr-sort-virtual (option)
  "Manages  sorting of buffers in Sunrise VIRTUAL mode. Since we cannot rely any
  more on all files in the buffer existing somewhere in the filesystem,  we  use
  the contents of the buffer itself for sorting its records, which must not only
  contain all the necessary data, but also must be  in  a  format  that  can  be
  easily  sorted.  See  the  variable  sr-virtual-listing-switches for the exact
  switches for ls that should be used."
  (let ((opt (string-to-char option))
        (beg) (end))
    (goto-char (point-max))
    (re-search-backward directory-listing-before-filename-regexp nil t)
    (end-of-line)
    (setq end (point))
    (goto-char (point-min))
    (re-search-forward directory-listing-before-filename-regexp nil t)
    (beginning-of-line)
    (setq beg (point))
    (toggle-read-only -1)
    (cond ((eq opt ?X) (sort-regexp-fields nil "^.*$" "[/.][^/.]+$" beg end))
          ((eq opt ?t) (sort-regexp-fields t "^.*$" "[0-9]\\{4\\}\\(-[0-9]\\{2\\}\\)\\{2\\} [0-2][0-9]:[0-5][0-9]" beg end))
          ((eq opt ?S) (sort-numeric-fields 5 beg end) (reverse-region beg end))
          (t  (sort-fields 8 beg end)))
    (toggle-read-only 1)))

;;; ============================================================================
;;; Passive & synchronized navigation functions:

(defmacro sr-in-other (form)
  "Executes the given form in the context of the passive pane. Helper macro for
   passive & synchronized navigation."
  `(progn
     (if sr-synchronized ,form)
     (sr-change-window)
     (condition-case description
         ,form
       (error (message (second description))))
     (sr-change-window)
     (sr-highlight)))

(defun sr-sync ()
  "Toggles the Sunrise synchronized navigation feature."
  (interactive)
  (setq sr-synchronized (not sr-synchronized))
  (mapc 'sr-mark-sync (list sr-left-buffer sr-right-buffer))
  (message (concat "Sync navigation is now "
                   (if sr-synchronized "ON" "OFF"))))

(defun sr-mark-sync (&optional buffer)
  "Changes  the  pretty  name  of  the sr major mode to 'Sunrise SYNC-LOCK' when
  operating in synchonized navigation mode."
  (save-window-excursion
    (if buffer
        (switch-to-buffer buffer))
    (setq mode-name (concat "Sunrise "
                            (if sr-synchronized "SYNC-NAV" "Commander")))))

;; This advertises synchronized navigation in all new buffers:
(add-hook 'sr-mode-hook 'sr-mark-sync)

(defun sr-next-line-other ()
  "Move the cursor down in the other pane."
  (interactive)
  (sr-in-other (dired-next-line 1)))

(defun sr-prev-line-other ()
  "Move the cursor up in the other pane."
  (interactive)
  (sr-in-other (dired-next-line -1)))

(defun sr-advertised-find-file-other ()
  "Open the file/directory selected in the other pane."
  (interactive)
  (if sr-synchronized
      (let ((target (sr-directory-name-proper (dired-get-filename))))
        (sr-change-window)
        (if (file-directory-p target)
            (sr-goto-dir (expand-file-name target))
          (if (y-or-n-p "Unable to synchronize. Disable sync navigation? ")
              (sr-sync)))
        (sr-change-window)
        (sr-advertised-find-file))
    (sr-in-other (sr-advertised-find-file))))

(defun sr-prev-subdir-other ()
  "Go to the previous subdirectory in the other pane."
  (interactive)
  (sr-in-other (sr-dired-prev-subdir)))

(defun sr-follow-file-other ()
  "Go to the same directory where the selected file is, but in the other pane."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (sr-in-other (sr-follow-file filename))))

(defun sr-history-prev-other ()
  "Changes  the  current  directory  to the previous one (if any) in the history
  list of the passive pane."
  (interactive)
  (sr-in-other (sr-history-prev)))

(defun sr-history-next-other ()
  "Changes the current directory to the next one (if any) in the history list of
  the passive pane."
  (interactive)
  (sr-in-other (sr-history-next)))

(defun sr-unmark-all-marks-other ()
  "Removes all marks from the passive pane"
  (interactive)
  (sr-in-other (dired-unmark-all-marks)))

;;; ============================================================================
;;; File manipulation functions:

(defun sr-editable-pane ()
  "Puts the current pane in Editable Dired mode (WDired)."
  (interactive)
  (sr-highlight 'sr-editing-path-face)
  (let* ((was-virtual (equal major-mode 'sr-virtual-mode))
	 (major-mode 'dired-mode))
    (wdired-change-to-wdired-mode)
    (if was-virtual
        (set (make-local-variable 'sr-virtual-buffer) t))))

(defun sr-readonly-pane (as-virtual)
  "Puts the current pane back in Sunrise mode."
  (when as-virtual
    (sr-virtual-mode)
    (sr-force-passive-highlight t))
  (sr-revert-buffer))

(defun sr-terminate-wdired (fun)
  "Restores the current pane's original mode after being edited with WDired."
  (ad-add-advice
   fun
   (ad-make-advice
    (intern (concat "sr-advice-" (symbol-name fun))) nil t
    `(advice
      lambda ()
      (if sr-running
	  (sr-save-aspect
	   (let ((was-virtual (local-variable-p 'sr-virtual-buffer))
                 (saved-point (point)))
	     (setq major-mode 'wdired-mode)
	     ad-do-it
	     (sr-readonly-pane was-virtual)
             (goto-char saved-point)))
	ad-do-it)))
   'around 'last)
  (ad-activate fun nil))
(sr-terminate-wdired 'wdired-finish-edit)
(sr-terminate-wdired 'wdired-abort-changes)

(defun sr-do-copy ()
  "Copies recursively selected files and directories from one pane to the other."
  (interactive)
  (save-excursion
    (let* (
           (selected-files (dired-get-marked-files nil))
           (files-count (length selected-files))
           (files-count-str (int-to-string files-count))
           (vtarget (sr-virtual-target))
           (target (or vtarget sr-other-directory))
           )
      (if (> files-count 0)
          (if (and (not vtarget)
                   (sr-equal-dirs default-directory sr-other-directory))
              (dired-do-copy)
            (when (y-or-n-p (concat "Copy " files-count-str " files to " target "? "))
              (if vtarget
                  (sr-copy-virtual)
                (progn
                  (dired-unmark-all-marks)
                  (sr-change-window)
                  (sr-copy-files selected-files default-directory)
                  (sr-revert-buffer)
                  (sr-change-window)))
              (message "%s" (concat "Done: "
                                    (int-to-string (length selected-files))
                                    " file(s) dispatched"))))
        (message "Empty selection. Nothing done.")))))

(defun sr-do-rename ()
  "Moves recursively selected files and directories from one pane to the other."
  (interactive)
  (if (sr-virtual-target)
      (error "Cannot move files to a VIRTUAL buffer, try (C)opying instead."))
  (save-excursion
    (let* (
           (selected-files (dired-get-marked-files nil))
           (files-count (length selected-files))
           (files-count-str (int-to-string files-count))
          )
      (if (> files-count 0)
          (if (sr-equal-dirs default-directory sr-other-directory)
              (dired-do-rename)
            (when (y-or-n-p (concat "Move " files-count-str
                                  " files to " sr-other-directory "? "))
              (sr-change-window)
              (sr-move-files selected-files default-directory)
              (sr-revert-buffer)
              (sr-change-window)
              (message "%s" (concat "Done: "
                                    (int-to-string (length selected-files))
                                    " file(s) dispatched"))))

        (message "Empty selection. Nothing done."))))
  (sr-revert-buffer))

(defun sr-do-symlink ()
  "Creates  symbolic  links  in  the  passive pane to all the currently selected
  files and directories in the active one."
  (interactive)
  (if (sr-equal-dirs default-directory sr-other-directory)
      (dired-do-symlink)
    (sr-link #'make-symbolic-link "Symlink" dired-keep-marker-symlink)))

(defun sr-do-relsymlink ()
  "Creates  relative  symbolic  links  in  the passive pane to all the currently
  selected files and directories in the active one."
  (interactive)
  (if (sr-equal-dirs default-directory sr-other-directory)
      (dired-do-relsymlink)
    (sr-link #'dired-make-relative-symlink
             "RelSymLink"
             dired-keep-marker-relsymlink)))

(defun sr-do-hardlink ()
  "Simply refuses to hardlink files to VIRTUAL buffers."
  (interactive)
  (if (sr-virtual-target)
      (error "Cannot hardlink files to a VIRTUAL buffer, try (C)opying instead.")
    (dired-do-hardlink)))

(defun sr-fast-backup-files ()
  "Makes  new  copies of all marked files (but not directories!) inside the same
  directory, each with extension .bak"
  (interactive)
  (dired-do-copy-regexp "$" ".bak")
  (sr-revert-buffer))

(defun sr-copy-files (file-path-list target-dir &optional do-overwrite)
  "Copies all files in file-path-list (list of full paths) to target dir."
  (setq target-dir (replace-regexp-in-string "/?$" "/" target-dir))
  (mapcar
   (function
    (lambda (f)
      (if (file-directory-p f)
          (let* (
                 (name (file-name-nondirectory f))
                 (initial-path (file-name-directory f))
                 )
            (sr-copy-directory initial-path name target-dir do-overwrite))
        (let* (
               (name (file-name-nondirectory f))
               (target-file (concat target-dir name))
               )
          (message "%s" (concat f " => " target-file))
          (if (file-exists-p target-file)
              (if (or (eq do-overwrite 'ALWAYS)
                      (setq do-overwrite (ask-overwrite target-file)))
                  (dired-copy-file f target-file t))
            (dired-copy-file f target-file t))))))
   file-path-list))

(defun sr-copy-directory (in-dir d to-dir do-overwrite)
  "Copies directory d in in-dir to to-dir, and recursively, all files too.
indir/d => to-dir/d"
  (setq d (replace-regexp-in-string "/?$" "/" d))
  (if (not (sr-overlapping-paths-p (concat in-dir d) to-dir))
      (progn
        (if (string= "" d)
            (setq to-dir (concat to-dir (sr-directory-name-proper in-dir))))
        ;; if directory in-dir/d does not exist:
        (if (not (file-exists-p (concat to-dir d)))
            (make-directory (concat to-dir d))) ; makes d in to-dir
        (let* (
               (files-in-d (append (sr-list-of-files (concat in-dir d))
                                   (sr-list-of-directories (concat in-dir d))))
               (file-paths-in-d
                (mapcar (lambda (f) (concat in-dir d f)) files-in-d))
               )
          (sr-copy-files file-paths-in-d (concat to-dir d) do-overwrite)))
    (error "ERROR: You cannot copy a directory into itself or one of its \
subdirectories")))

(defun sr-move-files (file-path-list target-dir &optional do-overwrite)
  "Moves all files in file-path-list (list of full paths) to target dir."
  (mapcar
   (function
    (lambda (f)
      (if (file-directory-p f)
          (progn
            (setq f (replace-regexp-in-string "/?$" "/" f))
            (let* (
                   (name (file-name-nondirectory f))
                   (target-subdir target-dir)
                   (initial-path (file-name-directory f))
                   )
              (if (string= "" name)
                  (setq target-subdir
                        (concat target-dir (sr-directory-name-proper f))))
              (if (file-exists-p target-subdir)
                  (if (or (eq do-overwrite 'ALWAYS)
                          (setq do-overwrite (ask-overwrite target-subdir)))
                      (sr-move-directory initial-path name target-dir do-overwrite))
                (sr-move-directory initial-path name target-dir do-overwrite))))
        (let* (
               (name (file-name-nondirectory f))
               (target-file (concat target-dir name))
               )
          (message "%s" (concat f " => " target-file))
          (if (file-exists-p target-file)
              (if (or (eq do-overwrite 'ALWAYS)
                      (setq do-overwrite (ask-overwrite target-file)))
                  (dired-rename-file f target-file t))
            (dired-rename-file f target-file t))) )))
   file-path-list))

(defun sr-move-directory (in-dir d to-dir do-overwrite)
  "Copies recursively the given directory d from in-dir to to-dir, then removes
the original one."
  (sr-copy-directory in-dir d to-dir do-overwrite)
  (let ((delete-dir (concat in-dir d)))
    (dired-delete-file delete-dir 'always)))

(defun sr-link (creator action marker)
  "Helper function for implementing sr-do-symlink and sr-do-relsymlink."
  (if (sr-virtual-target)
      (error "Cannot link files to a VIRTUAL buffer, try (C)opying instead.")
    (dired-create-files creator action (dired-get-marked-files nil)
                        #'(lambda (from)
                            (setq from (replace-regexp-in-string "/$" "" from))
                            (if (file-directory-p from)
                                (setq from (sr-directory-name-proper from))
                              (setq from (file-name-nondirectory from)))
                            (expand-file-name from sr-other-directory))
                        marker)))

(defun sr-virtual-target ()
  "If the passive pane is in VIRTUAL mode returns its name as a string,
   otherwise returns nil."
  (save-window-excursion
    (if (equal sr-selected-window 'left)
        (switch-to-buffer sr-right-buffer)
      (switch-to-buffer sr-left-buffer))
    (if (equal major-mode 'sr-virtual-mode)
        (or (buffer-file-name) "Sunrise VIRTUAL buffer")
      nil)))

(defun sr-copy-virtual ()
  "Manages  copying  of  files/directories  to  buffers  in  VIRTUAL  mode. Like
  sorting, this operation depends on  the  variable  sr-virtual-listing-switches
  set  to the right value. See the documentation of function sr-sort-virtual for
  more details."
  (let ((fileset (dired-get-marked-files nil))
        indentation)
    (sr-change-window)
    (sr-end-of-buffer)
    (beginning-of-line)
    (re-search-forward "\\S-" nil t)
    (setq indentation (- (current-column) 1))
    (dired-next-line 1)
    (toggle-read-only -1)
    (mapc (lambda (file)
            (insert-char 32 indentation)
            (setq file (replace-regexp-in-string "/$" "" file))
            (insert-directory file sr-virtual-listing-switches)
            (sr-end-of-buffer)
            (dired-next-line 1))
          fileset)
    (unwind-protect
        (kill-line)
      (progn
        (toggle-read-only 1)
        (sr-revert-buffer)
        (sr-change-window)
        (dired-unmark-all-marks)))))

(defun ask-overwrite (file-name)
  "Asks whether to overwrite a given file."
  (y-n-or-a-p (concat "File " file-name " exists. OK to overwrite? ")))

(defun y-n-or-a-p (prompt)
  "Prompts  for  an answer to an alternative of the type y/n/a (where 'a' stands
  for 'always') and returns t if the answer is 'y', nil if the answer is 'n'  or
  the symbol ALWAYS."
  (setq prompt (concat prompt "([y]es, [n]o or [a]lways)"))
  (let ((resp -1))
    (while (not (memq resp '(?y ?Y ?n ?N ?a ?A)))
      (setq resp (read-event prompt))
      (setq prompt "Please answer [y]es, [n]o or [a]lways "))
    (if (>= resp 97)
        (setq resp (- resp 32)))
    (cond ((eq resp ?Y) t)
          ((eq resp ?A) 'ALWAYS)
          (t nil))))

(defun sr-overlapping-paths-p (dir1 dir2)
  "Determines whether the directory dir2 is located inside the directory dir1."
  (if (>= (length dir2) (length dir1))
      (equal (substring dir2 0 (length dir1)) dir1)
      nil))

(defun sr-list-of-directories (dir)
 "Return  a  list of directories in DIR. Each entry in the list is a string. The
 list does not include the current directory and the parent directory."
 (let (result)
   (setq result
         (sr-filter (function (lambda (x) (not (or (equal x ".") (equal x "..")))))
                    (sr-filter
                     (function (lambda (x)
                                 (file-directory-p (concat dir "/" x))))
                     (directory-files dir))))
   (mapcar (lambda (x) (concat x "/")) result)))

(defun sr-list-of-files (dir)
  "Return a list of regular files in DIR. Each entry in the list is a string."
  (sr-filter
   (function (lambda (x)
               (file-regular-p (concat dir "/" x))))
   (directory-files dir)))

(defun sr-filter (p x)
  "Filter  takes two arguments: a predicate P and a list X.  Return the elements
  of the list X that satisfy the predicate P."
  (let ((res-list nil))
    (while x
      (if (apply p (list (car x)))
          (setq res-list (cons (car x) res-list)))
      (setq x (cdr x)))
    (reverse res-list)))

(defun sr-find-last-point (str)
  "Return the position of the last point in the string str. Do not allow to pass
  '/' while looking for the point. If no point is found under these  conditions,
  return nil."
  (let ((idx (- (length str) 1)))
    (while (and (>= idx 0)
                (not (eq (aref str idx) ?.))
                (not (eq (aref str idx) ?/)))
      (setq idx (- idx 1)))
    (if (and (>= idx 0) (eq (aref str idx) ?.)) idx nil)))

(defun sr-directory-name-proper (file-path)
  "Takes  as  input  an absolute or relative, forward slash terminated path to a
  directory.  Return the proper name of the directory, without initial path. The
  remaining part of file-path can be accessed by the function parent-directory."
  (let (
        (file-path-1 (substring file-path 0 (- (length file-path) 1)))
        (lastchar (substring file-path (- (length file-path) 1)))
        )
    (concat (file-name-nondirectory file-path-1) lastchar)))

;;; ============================================================================
;;; Directory and file comparison functions:

(defun sr-compare-dirs()
  "Compares paned directories between themselves."
  (interactive)
  (dired-compare-directories sr-other-directory (ask-compare-dirs-predicate)))

(defun ask-compare-dirs-predicate ()
  "Prompts for the criterion to use for comparing two directories."
  (let (
        (resp -1)
        (prompt "Compare by (d)ate, (s)ize, date_(a)nd_size or (c)ontents? ")
       )
    (while (not (memq resp '(?d ?D ?s ?S ?a ?A ?c ?C)))
      (setq resp (read-event prompt))
      (setq prompt "Please select: Compare by (d)ate, (s)ize, date_(a)nd_size \
or (c)ontents? "))
    (if (>= resp 97)
        (setq resp (- resp 32)))
    (cond ((eq resp ?D)
           (list 'not (list '= 'mtime1 'mtime2)))
          ((eq resp ?S)
           (list 'not (list '= 'size1 'size2)))
          ((eq resp ?C)
           (list 'not (list 'string=
                            (list 'sr-md5 'file1)
                            (list 'sr-md5 'file2))))
          (t
           (list 'or
                 (list 'not (list '= 'mtime1 'mtime2))
                 (list 'not (list '= 'size1 'size2)))))))

(defun sr-md5 (file-alist)
  "Builds  and  executes a shell command to calculate the MD5 sum of the file
  referred to by the given file list, in which the second element is the name
  of the file."
  (let* ((filename (second file-alist))
        (md5-command
         (replace-regexp-in-string "%f" filename sr-md5-shell-command)))
    (if (file-directory-p filename)
        ""
      (shell-command-to-string md5-command))))

(defun sr-diff ()
  "Runs diff on the top two marked files in both panes."
  (interactive)
  (eval (sr-diff-form 'diff))
  (sr-scrollable-viewer (get-buffer "*Diff*")))

(defun sr-ediff ()
  "Runs ediff on the two top marked files in both panes."
  (interactive)
  (eval (sr-diff-form 'ediff)))

(add-hook 'ediff-before-setup-windows-hook
          (lambda () (setq sr-ediff-on t)))

(add-hook 'ediff-quit-hook
          (lambda ()
            (setq sr-ediff-on nil)
            (when sr-running
              (if (buffer-live-p sr-restore-buffer)
                  (switch-to-buffer sr-restore-buffer))
              (delete-other-windows)
              (sr-setup-windows))))

(defun sr-diff-form (fun)
  "Determines the arguments to be passed to the diff function and returns the
  form to evaluate to perform the comparison."
  (let ((this (sr-pop-mark)) (other nil))
    (unless this
      (setq this (car (dired-get-marked-files t))))
    (if (sr-equal-dirs default-directory sr-other-directory)
        (setq other (sr-pop-mark))
      (progn
        (sr-change-window)
        (setq other (sr-pop-mark))
        (sr-change-window)
        (setq other (or other this))))
    (setq this (concat default-directory this))
    (setq other (concat sr-other-directory other))
    (list fun this other)))

(defun sr-pop-mark ()
  "Pops the first mark in the current dired buffer."
  (let ((marks (dired-get-marked-files t nil nil t)))
    (if (< 1 (length marks))
        (progn
          (dired-unmark-all-marks)
          (if (not (equal t (car marks)))
              (progn
                (mapc (lambda (x)
                          (dired-mark-files-regexp
                           (concat "^" (regexp-quote x) "$")))
                        (cdr marks))
                (car marks))
            (second marks)))
      nil)))

;;; ============================================================================
;;; File search functions:

(defun sr-find-apply (fun pattern)
  "Helper function for functions sr-find, sr-find-name and sr-find-grep."
  (let* ((suffix (if (eq 'w32 window-system) " {} ;" " \\{\\} \\;"))
         (find-ls-option
          (cons
           (concat "-exec ls -d " sr-virtual-listing-switches suffix)
           "ls -ld")))
    (sr-save-aspect
     (apply fun (list default-directory pattern))
     (sr-virtual-mode)
     (sr-keep-buffer))))

(defun sr-find (pattern)
  "Runs find-dired passing the current directory as first parameter."
  (interactive "sRun find (with args): ")
  (sr-find-apply 'find-dired pattern))

(defun sr-find-name (pattern)
  "Runs find-name-dired passing the current directory as first parameter."
  (interactive "sFind name pattern: ")
  (sr-find-apply 'find-name-dired pattern))

(defun sr-find-grep (pattern)
  "Runs find-grep-dired passing the current directory as first parameter."
  (interactive "sFind files containing pattern: ")
  (sr-find-apply 'find-grep-dired pattern))

(defun sr-locate ()
  "Runs locate with the necessary options to produce a buffer that can be put in
   sunrise virtual mode."
  (interactive)
  (sr-save-aspect
   (switch-to-buffer "*Locate*")
   (let ((locate-prompt-for-command t)
         (locate-filename-indentation 2)
         (locate-make-command-line
          (lambda (arg)
            (list "locate" arg "| xargs ls -d" sr-virtual-listing-switches))))
     (call-interactively 'locate))
   (sr-virtual-mode)
   (sr-keep-buffer)))

(defun sr-recent-files ()
  "Displays the history of recent files maintained by recentf in sunrise virtual
   mode."
  (interactive)
  (if (not (featurep 'recentf))
      (error "ERROR: Feature recentf not available!"))

  (sr-save-aspect
   (let ((dired-actual-switches dired-listing-switches))
     (sr-switch-to-clean-buffer "*Recent Files*")
     (insert "Recently Visited Files: \n")
     (dolist (file recentf-list)
       (condition-case nil
           (insert-directory file sr-virtual-listing-switches nil nil)
         (error (ignore))))
     (sr-virtual-mode)
     (sr-keep-buffer))))

(defun sr-recent-directories ()
  "Displays the history of directories recently visited in the current pane."
  (interactive)
  (sr-save-aspect
   (let ((hist (cdr (assoc sr-selected-window sr-history-registry)))
         (dired-actual-switches dired-listing-switches)
         (pane-name (capitalize (symbol-name sr-selected-window)))
         (beg))
     (sr-switch-to-clean-buffer (concat "*" pane-name " Pane History*"))
     (insert (concat "Recent Directories in " pane-name " Pane: \n"))
     (dolist (dir hist)
       (condition-case nil
           (when (and dir (file-exists-p dir))
             (setq dir (replace-regexp-in-string "\\(.\\)/?$" "\\1" dir))
             (setq beg (point))
             (insert-directory dir sr-virtual-listing-switches nil nil)
             (dired-align-file beg (point)))
         (error (ignore))))
     (sr-virtual-mode))))

(defun sr-switch-to-clean-buffer (name)
  (sr-alternate-buffer (switch-to-buffer name))
  (kill-region (point-min) (point-max)))

(defun sr-pure-virtual ()
  "Creates a new empty buffer in Sunrise VIRTUAL mode."
  (interactive)
  (sr-save-aspect
   (let ((dir (directory-file-name (dired-current-directory)))
         (buff (generate-new-buffer-name (buffer-name (current-buffer))))
         (dispose (current-buffer)))
     (switch-to-buffer buff)
     (goto-char (point-min))
     (insert (concat "  " dir) ":\n")
     (insert " Pure VIRTUAL buffer: \n")
     (insert "  drwxrwxrwx 0 ? ? 0000 0000-00-00 00:00 ./\n")
     (sr-virtual-mode)
     (sr-keep-buffer)
     (unless (sr-equal-dirs sr-this-directory sr-other-directory)
       (kill-buffer dispose)))))

;; This cleans up the current pane after deletion from the history of recent
;; files:
(defadvice dired-do-flagged-delete
  (after sr-advice-dired-do-flagged-delete (&optional nomessage))
  (if (string= (buffer-name) "*Recent Files*")
      (sr-recent-files)))
(ad-activate 'dired-do-flagged-delete)

(defun sr-dired-do-apply (dired-fun)
  "Helper function for implementing sr-do-query-replace-regexp and Co."
  (let ((buff (current-buffer)))
    (sr-quit)
    (switch-to-buffer buff)
    (call-interactively dired-fun)))

(defun sr-do-query-replace-regexp ()
  "Forces Sunrise to quit before executing dired-do-query-replace-regexp."
  (interactive)
  (sr-dired-do-apply 'dired-do-query-replace-regexp))

(defun sr-do-search ()
  "Forces Sunrise to quit before executing dired-do-search."
  (interactive)
  (sr-dired-do-apply 'dired-do-search))

;;; ============================================================================
;;; TI (Terminal Integration) and CLEX (Command Line EXpansion) functions:

(defun sr-term (&optional cd newterm)
  "Runs  terminal  in  a  new  buffer  (or  switches to an existing one). If the
  optional parameter  cd  is  provided  and  equal  t  sends  automatically  the
  appropriate command to change directory to the current one in the active pane."
  ;; Dynamic function -- redefines itself the first time it's executed:
  (interactive)
  (if (string= sr-terminal-program "eshell")
      (progn
        (add-hook 'eshell-mode-hook
                  '(lambda () (sr-define-ti-keys eshell-mode-map)))
        (defun sr-term (&optional cd newterm)
          (interactive)
          (sr-term-eshell cd newterm)))
    (progn
      (add-hook 'term-mode-hook
                '(lambda () (sr-define-ti-keys term-mode-map)))
      (defun sr-term (&optional cd newterm)
        (interactive)
        (sr-term-extern cd newterm))))
  (sr-term))

(defun sr-term-cd ()
  "Runs  terminal  in  a new buffer (or switches to an existing one) and cd’s to
  the current directory in the active pane."
  (interactive)
  (sr-term t))

(defun sr-term-cd-newterm ()
  "Opens a  NEW terminal (never switches to an existing one) in a new buffer and
  cd’s to the current directory in the active pane."
  (interactive)
  (sr-term t t))

(defmacro sr-term-excursion (newterm form)
  "Helper  macro.  Takes  care  of  the  common mechanics of launching a new (or
  switching to an existing) terminal from Sunrise."
  `(let ((buffer))
     (sr-select-viewer-window)
     ,form
     (setq buffer (current-buffer))
     (setq cd (or cd (null sr-ti-openterms)))
     (if (not (eq buffer (first sr-ti-openterms)))
         (push (current-buffer) sr-ti-openterms)
       (if ,newterm
           (sr-ti-newterm)))))

(defun sr-term-extern (&optional cd newterm)
  "This is the implementation of sr-term for external terminal programs."
  (let ((dir default-directory))
    (sr-term-excursion newterm (term sr-terminal-program))
    (when cd
      (term-send-raw-string
       (concat "cd " (shell-quote-wildcard-pattern dir) "
")))))

(defun sr-term-eshell (&optional cd newterm)
  "This is the implementation of sr-term when using eshell."
  (let ((dir default-directory))
    (sr-term-excursion newterm (eshell))
    (when cd
      (insert (concat "cd " (shell-quote-wildcard-pattern dir)))
      (eshell-send-input))))

(defmacro sr-ti (form)
  "Puts  the  given  form  in the context of the selected pane. Helper macro for
   implementing terminal integration in Sunrise."
  `(if sr-running
       (progn
         (sr-select-window sr-selected-window)
         (unwind-protect
             ,form
           (progn
             (sr-highlight)
             (sr-select-viewer-window))))))

(defun sr-ti-previous-line ()
  "Runs previous-line on active pane from the terminal window."
  (interactive)
  (sr-ti (forward-line -1)))

(defun sr-ti-next-line ()
  "Runs next-line on active pane from the terminal window."
  (interactive)
  (sr-ti (forward-line 1)))

(defun sr-ti-select ()
  "Runs dired-advertised-find-file on active pane from the terminal window."
  (interactive)
  (sr-ti (sr-advertised-find-file)))

(defun sr-ti-mark ()
  "Runs dired-mark on active pane from the terminal window."
  (interactive)
  (sr-ti (dired-mark 1)))

(defun sr-ti-unmark ()
  "Runs dired-unmark-backward on active pane from the terminal window."
  (interactive)
  (sr-ti (dired-unmark-backward 1)))

(defun sr-ti-prev-subdir ()
  "Runs dired-prev-subdir on active pane from the terminal window."
  (interactive)
  (sr-ti (sr-dired-prev-subdir)))

(defun sr-ti-unmark-all-marks ()
  "Removes all marks on active pane from the terminal window."
  (interactive)
  (sr-ti (dired-unmark-all-marks)))

(defun sr-ti-change-window ()
  "Switches focus to the currently active pane."
  (interactive)
  (sr-select-window sr-selected-window))

(defun sr-ti-newterm ()
  "Opens a new terminal after renaming the previous one."
  (interactive)
  (let (new-name)
    (rename-uniquely)
    (setq new-name (buffer-name))
    (sr-term)
    (message "%s" (concat "Previous terminal renamed to " new-name))))

(defun sr-ti-restore-previous-term ()
  "Renames back the last open terminal (if any) to the default terminal buffer
   name after the current one is closed."
  (let ((found nil)
        (name (buffer-name)))
    (while (and sr-ti-openterms
                (not (buffer-live-p (first sr-ti-openterms))))
      (pop sr-ti-openterms))
      (when (and (string= (buffer-name (first sr-ti-openterms)) name)
                 (first sr-ti-openterms)
                 (pop sr-ti-openterms)
                 (buffer-live-p (first sr-ti-openterms)))
        (rename-uniquely)
        (set-buffer (first sr-ti-openterms))
        (rename-buffer name))))
(add-hook 'kill-buffer-hook 'sr-ti-restore-previous-term)

(defmacro sr-clex (pane form)
  "Executes the given form in the context of the given pane. Helper macro for
   implementing command line expansion in Sunrise."
  `(save-window-excursion
     (select-window (symbol-value (sr-symbol ,pane 'window)))
     ,form))

(defun sr-clex-marked (pane)
  "Returns a string containing the list of marked files in the given pane."
  (sr-clex
   pane
   (mapconcat 'shell-quote-wildcard-pattern (dired-get-marked-files) " ")))

(defun sr-clex-file (pane)
  "Returns the currently selected file in the given pane."
  (sr-clex
   pane
   (concat (shell-quote-wildcard-pattern (dired-get-filename)) " ")))

(defun sr-clex-marked-nodir (pane)
  "Returns  a list containing the names of all the currently marked files in the
  given pane, without the directory prepended."
  (sr-clex
   pane
   (mapconcat 'shell-quote-wildcard-pattern
              (dired-get-marked-files 'no-dir) " ")))

(defun sr-clex-dir (pane)
  "Returns the current directory in the given pane."
  (sr-clex
   pane
   (concat (shell-quote-wildcard-pattern default-directory) " ")))

(defun sr-clex-start ()
  "Starts a new CLEX operation. Registers sr-clex-commit as a local
   after-change-function."
  (interactive)
  (if sr-clex-on
      (progn
        (setq sr-clex-on nil)
        (delete-overlay sr-clex-hotchar-overlay))
    (progn
      (insert-char ?% 1)
      (if sr-running
          (progn
            (add-hook 'after-change-functions 'sr-clex-commit nil t)
            (setq sr-clex-on t)
            (setq sr-clex-hotchar-overlay (make-overlay (point) (1- (point))))
            (overlay-put sr-clex-hotchar-overlay 'face 'sr-clex-hotchar-face)
            (message "Sunrise: CLEX is now ON for keys: m f n d M F N D %%"))))))

(defun sr-clex-commit (&optional beg end range)
  "Commits the current CLEX operation (if any). This function is added to the
   local after-change-functions list of the buffer by sr-clex-start."
  (interactive)
  (if sr-clex-on
      (progn
        (setq sr-clex-on nil)
        (delete-overlay sr-clex-hotchar-overlay)
        (let ((xchar (char-before))
              (expansion))
          (setq expansion
                (cond ((eq xchar ?m) (sr-clex-marked       'left))
                      ((eq xchar ?f) (sr-clex-file         'left))
                      ((eq xchar ?n) (sr-clex-marked-nodir 'left))
                      ((eq xchar ?d) (sr-clex-dir          'left))
                      ((eq xchar ?M) (sr-clex-marked       'right))
                      ((eq xchar ?F) (sr-clex-file         'right))
                      ((eq xchar ?N) (sr-clex-marked-nodir 'right))
                      ((eq xchar ?D) (sr-clex-dir          'right))
                      (t nil)))
          (if expansion
              (progn
                (kill-backward-chars 2)
                (insert expansion)))))))

(defvar sr-term-keys '(([M-up]          . sr-ti-previous-line)
                       ([A-up]          . sr-ti-previous-line)
                       ("\M-P"          . sr-ti-previous-line)
                       ([M-down]        . sr-ti-next-line)
                       ([A-down]        . sr-ti-next-line)
                       ("\M-N"          . sr-ti-next-line)
                       ("\M-\C-m"       . sr-ti-select)
                       ("\C-\M-j"       . sr-ti-select)
                       ([M-return]      . sr-ti-select)
                       ("\M-M"          . sr-ti-mark)
                       ([M-backspace]   . sr-ti-unmark)
                       ("\M-\d"         . sr-ti-unmark)
                       ("\M-J"          . sr-ti-prev-subdir)
                       ("\M-U"          . sr-ti-unmark-all-marks)
                       ([(control tab)] . sr-ti-change-window)
                       ("\C-c\t"        . sr-ti-change-window)
                       ("\C-ct"         . sr-ti-newterm)
                       ("%"             . sr-clex-start))
  "Keybindings for terminal integration and command line expansion")

(defun sr-define-ti-keys (mode-map)
  (mapcar (lambda (key)
            (define-key mode-map (car key) (cdr key)))
          sr-term-keys))

;;; ============================================================================
;;; Miscellaneous functions:

(defun sr-keep-buffer ()
  "Keeps  the currently selected buffer as one of the panes, even if it does not
  belong to the pane's history ring. Useful for maintaining the  contents  of  a
  pane during layout switching."
  (if (equal sr-selected-window 'left)
      (setq sr-left-buffer (current-buffer))
    (setq sr-right-buffer (current-buffer))))

(defun sr-scrollable-viewer (buffer)
  "Sets the other-window-scroll-buffer variable to the given buffer (or nil)."
  (setq other-window-scroll-buffer buffer)
  (if buffer
      (message "QUICK VIEW: Press C-M-v, S-C-M-v to scroll up/down and C-u v (or C-u o) to dismiss")))

(defun sr-describe-mode ()
  "Calls describe-mode and makes the resulting buffer C-M-v scrollable."
  (interactive)
  (describe-mode)
  (sr-scrollable-viewer (get-buffer "*Help*")))

(defun sr-equal-dirs (dir1 dir2)
  "Determines whether two directory paths represent the same directory."
  (string= (expand-file-name dir1) (expand-file-name dir2)))

(defun sr-summary ()
  "Summarize basic Sunrise commands and show recent dired errors."
  (interactive)
  (dired-why)
  (message "C-opy, R-ename, D-elete, v-iew, q-uit, U-p, m-ark, u-nmark, h-elp"))

;;; ============================================================================
;;; Font-Lock colors & styles:

(defmacro sr-rainbow (symbol spec regexp)
  `(progn
     (defface ,symbol '((t ,spec)) "Sunrise rainbow face" :group 'sunrise)
     (font-lock-add-keywords 'sr-mode '((,regexp 1 (quote ,symbol))))
     (font-lock-add-keywords 'sr-virtual-mode '((,regexp 1 (quote ,symbol))))))

(sr-rainbow sr-html-face              (:foreground "DarkOliveGreen")        "\\(^..[^d].*\\.x?html?$\\)")
(sr-rainbow sr-xml-face               (:foreground "DarkGreen")             "\\(^..[^d].*\\.\\(xml\\|xsd\\|xslt?\\|wsdl\\)$\\)")
(sr-rainbow sr-log-face               (:foreground "brown")                 "\\(^..[^d].*\\.log$\\)")
(sr-rainbow sr-compressed-face        (:foreground "magenta")               "\\(^..[^d].*\\.\\(zip\\|bz2\\|t?gz\\|[zZ]\\|[jwers]?ar\\|xpi\\)$\\)")
(sr-rainbow sr-packaged-face          (:foreground "DarkMagenta")           "\\(^..[^d].*\\.\\(deb\\|rpm\\)$\\)")
(sr-rainbow sr-encrypted-face         (:foreground "DarkOrange1")           "\\(^..[^d].*\\.\\(gpg\\|pgp\\)$\\)")

(sr-rainbow sr-directory-face         (:foreground "blue1" :bold t)         "\\(^..d.*/$\\)")
(sr-rainbow sr-symlink-face           (:foreground "DeepSkyBlue" :italic t) "\\(^..l.*[^/]$\\)")
(sr-rainbow sr-symlink-directory-face (:foreground "blue1" :italic t)       "\\(^..l.*/$\\)")
(sr-rainbow sr-alt-marked-dir-face    (:foreground "DeepPink" :bold t)      "\\(^[^ *D].d.*$\\)")
(sr-rainbow sr-alt-marked-file-face   (:foreground "DeepPink")              "\\(^[^ *D].[^d].*$\\)")
(sr-rainbow sr-marked-dir-face        (:foreground "red" :bold t)           "\\(^[*D].d.*$\\)")
(sr-rainbow sr-marked-file-face       (:foreground "red")                   "\\(^[*D].[^d].*$\\)")

(provide 'sunrise-commander)

;;; sunrise-commander.el ends here
