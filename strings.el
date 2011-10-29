;;; strings.el --- Miscellaneous string functions.
;;
;; Filename: strings.el
;; Description: Miscellaneous string functions.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2011, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 17:09:08 1996
;; Version: 21.0
;; Last-Updated: Sat Mar  5 14:32:29 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 514
;; URL: http://www.emacswiki.org/cgi-bin/wiki/strings.el
;; Keywords: internal, strings, text
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `misc-fns', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;        Miscellaneous string functions.
;;
;;  You may want to put this in your `~/.emacs' file, to erase the
;;  minibuffer when it is inactive and `minibuffer-empty-p':
;;
;;   (require 'strings)
;;   (add-hook 'pre-command-hook 'erase-nonempty-inactive-minibuffer))
;;
;;
;;  Macros defined here:
;;
;;    `empty-name-p', `non-empty-name-p'.
;;
;;  Functions defined here:
;;
;;    `absdiff', `buffer-alist', `concat-w-faces',
;;    `current-d-m-y-string', `current-line-string',
;;    `display-in-minibuffer', `display-lines-containing',
;;    `echo-in-buffer', `empty-name-p', `erase-inactive-minibuffer',
;;    `erase-nonempty-inactive-minibuffer', `frame-alist',
;;    `insert-in-minibuffer', `minibuffer-empty-p',
;;    `non-empty-name-p', `ordinal-suffix', `pick-some-words',
;;    `read-any-variable', `read-number', `region-description',
;;    `set-minibuffer-empty-p', `string-w-face',
;;    `symbol-name-before-point', `word-before-point'.
;;
;;  Variables defined here:
;;
;;    `minibuffer-empty-p'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `read-buffer'   - Uses `completing-read'.
;;  `read-variable' - Uses `symbol-nearest-point' & `completing-read'
;;                    to get the default.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/04 dadams
;;     Removed autoload cookies from non-interactive functions.
;; 2010/06/28 dadams
;;     read-buffer: Don't provide a default if none given, unless interactive.
;; 2010/02/22 dadams
;;     read-buffer: Update for Emacs 23 (pretest):
;;       Handle read-buffer-function and read-buffer-completion-ignore-case.
;; 2010/01/12 dadams
;;     current-line-string, minibuffer-empty-p, erase-inactive-minibuffer:
;;       save-excursion + set-buffer -> with-current-buffer.
;;     insert-in-minibuffer: set-buffer -> with-current-buffer.
;; 2009/09/09 dadams
;;     (non-)empty-name-p: Got rid of old-style backquote syntax.
;; 2008/05/12 dadams
;;     read-buffer: Don't use (buffer-alist t) - don't exclude hidden buffers.
;; 2007/04/20 dadams
;;     read-number: Updated for Emacs 22.
;; 2006/08/22 dadams
;;     buffer-alist: Added nospacep arg.  Use t for it in read-buffer.
;; 2005/10/31 dadams
;;     Added: read-any-variable.
;;     Use nil as init-value arg in calls to completing-read, everywhere.
;; 2005/07/31 dadams
;;     Protected read-number with fboundp.
;; 2005/07/28 dadams
;;     concat-w-faces: Mention in doc string that it is now obsolete.
;; 2005/05/28 dadams
;;     read-buffer: Use other-buffer, if another-buffer is not available.
;; 2004/09/21 dadams
;;     Added buffer-alist (from elect-mbuf.el), so file would be standalone.
;;     Removed require of elect-mbuf.el (circular dependency).
;; 1999/03/17 dadams
;;     Added: read-buffer, read-number, read-variable, frame-alist.
;; 1996/03/26 dadams
;;     1. Added string-w-face, concat-w-faces.
;;     2. insert-in-minibuffer: Use concat-w-faces.
;; 1996/03/19 dadams
;;     Added current-d-m-y-string.
;; 1996/03/15 dadams
;;     minibuffer-empty-p, erase-inactive-minibuffer, insert-in-minibuffer:
;;       Use minibuffer-window, not minibuffer-frame.
;; 1996/03/12 dadams
;;     1. Added: minibuffer-empty-p, set-minibuffer-empty-p,
;;               erase-nonempty-inactive-minibuffer, erase-inactive-minibuffer.
;;     2. insert-in-minibuffer: Reset set-minibuffer-empty-p to nil.
;; 1996/02/22 dadams
;;     insert-in-minibuffer: Protect against read-from-string error.
;; 1996/02/22 dadams
;;     Added display-lines-containing.
;; 1996/02/14 dadams
;;     insert-in-minibuffer: Corrected bug when `"' in string itself.
;; 1996/02/13 dadams
;;     symbol-name-before-point: Skip non-symbol, non-word syntax at end of name too.
;;     (Was not removing  "',"  in  "`toto',")
;; 1996/01/08 dadams
;;     Added insert-in-minibuffer, erase-inactive-minibuffer, display-in-minibuffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; psetq (plus, for Emacs <20: cadr, when, unless)

(require 'thingatpt nil t) ;; (no error if not found): symbol-at-point
(require 'thingatpt+ nil t)  ;; (no error if not found): symbol-nearest-point
(require 'misc-fns nil t) ;; (no error if not found): another-buffer


(defvar read-buffer-completion-ignore-case) ; Quiet the byte-compiler.

;;;;;;;;;;;;;;;;;;;;;;;


;; Taken from `wimpy-del.el'.
(defsubst absdiff (m n)
  "Absolute value of the difference between two numbers.
M and N are the numbers."
  (if (< m n) (- n m) (- m n)))

;;;###autoload
(defmacro empty-name-p (name)
  "Nil if NAME is nil or \"\", else t."
  `(or (null ,name) (string= "" ,name)))

;;;###autoload
(defmacro non-empty-name-p (name)       ; Error if NAME is neither nil nor a string.
  "NAME if non-nil and not \"\", else nil."
  `(and ,name (not (string= "" ,name)) ,name))

;; Stolen from `diary.el' (`diary-ordinal-suffix').
(defun ordinal-suffix (n)
  "Ordinal suffix for N.  That is, `st', `nd', `rd', or `th', as appropriate."
  (if (or (memq (% n 100) '(11 12 13)) (< 3 (% n 10)))
      "th"
    (aref ["th" "st" "nd" "rd"] (% n 10))))

;; Stolen from `wimpy-del.el'.
(defun pick-some-words (pos direction limit)
  "Get string from buffer of at most LIMIT chars, with one end at position POS.
Tries to fit as many words into the string as possible.  If it cannot fit even
one word, it will get LIMIT characters.  DIRECTION = nil for forward, non-nil
for backward."
  (save-excursion
    (goto-char pos)
    (let (p q)
      (if direction
          (backward-word 1)
        (forward-word 1))
      (if (> (absdiff (point) pos) limit)
          (buffer-substring pos (+ (if direction (- limit) limit) pos))
        (setq p (point) q t)
        (while (and q (not (eobp)) (not (bobp)))
          (if direction
              (backward-word 1)
            (forward-word 1))
          (if (<= (absdiff (point) pos) limit)
              (setq p (point))
            (goto-char p)
            (setq q nil)))
        (buffer-substring pos (point))))))

;; Stolen from `wimpy-del.el'
(defun region-description (width &optional prefix suffix begin end)
  "Return a string containing a one-line description of the region.
WIDTH arg is max length of string (must be at least 20 chars).
Optional args PREFIX and SUFFIX are strings (default: \"\") added to msg ends.
They count towards its length.
Optional args BEGIN and END delimit the region to use."
  (unless prefix (setq prefix ""))
  (unless suffix (setq suffix ""))
  (when (and begin (not end)) (setq end (point)))
  (cond (begin ;; Use arg-supplied region
         (psetq begin (min begin end)
                end   (max begin end)))
        (t ;; Use current region.
         (setq begin (min (point) (mark)))
         (setq end   (max (point) (mark)))))
  ;;(message "%d `%s' `%s' %d %d" width prefix suffix begin end)
  (cond ((< (- end begin) (- width 2))
         (concat "\"" (buffer-substring begin end) "\""))
        (t
         (let* ((chars-string (format "     (%d chars)" (- end begin)))
                (space-for-messages (+ (length prefix) (length suffix)
                                       (length chars-string)))
                (space-for-quote (/ (max 0 (- width space-for-messages)) 2))
                (beg-words (pick-some-words begin nil space-for-quote))
                (end-words (pick-some-words end   t   space-for-quote)))
           (concat prefix beg-words "   ...   "
                   end-words suffix chars-string)))))

;; From `header2.el'.
(defun current-d-m-y-string ()
  "Return string of current day, month, and year, in form \"dd-mon-year\"."
  (let ((str (current-time-string)))
    (concat (if (equal ?\  (aref str 8))
                (substring str 9 10)
              (substring str 8 10))
            "-" (substring str 4 7) "-" (substring str 20 24))))

;; Adapted from file `intes.el.2'.
(defun current-line-string (&optional buffer)
  "Return current line of text in BUFFER as a string."
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (buffer-substring (progn (end-of-line 1) (point))
                      (progn (beginning-of-line 1) (point)))))

;;;###autoload
(defun display-lines-containing (buffer string &optional flush-p)
  "Display in BUFFER the lines of `current-buffer' containing STRING.
See also command `occur' which does this and much more.  As this does
less, it can be useful if you intend to manipulate the contents of
BUFFER, not just use it to find things in the `current-buffer'.

BUFFER is a buffer or its name (a string).  STRING is a string.
Optional third argument FLUSH-P, if non-nil, means to display lines of
`current-buffer' that do *not* contain STRING.
Interactively:
  BUFFER defaults to buffer \"*Lines Containing*\".
  STRING is read in the minibuffer, and defaults to `current-line-string'.
  FLUSH-P is the prefix argument."
  (interactive
   (list (get-buffer-create "*Lines Containing*")
         (read-from-minibuffer "Lines containing: "
                               (current-line-string) nil nil
                               (cons minibuffer-history 1))
         current-prefix-arg))
  (setq buffer (get-buffer-create buffer)) ; Convert possible string to buffer.
  (let ((bufstring (buffer-string)))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (insert bufstring)
    (goto-char (point-min))
    (if flush-p
        (flush-lines string)
      (keep-lines string))
    (set-buffer-modified-p nil)))

(defun word-before-point ()
  "Return the word at (or before) the cursor, as a string.
\"Word\" is as defined by `forward-word'.
Note: It is possible for the symbol found to be on a previous line.

Some related functions:
  `symbol-name-nearest-point' returns the name of `symbol-nearest-point'
    as a string, or \"\" if none.
  `symbol-name-before-point'  returns the string naming the symbol at or
    before the cursor (even if it is on a previous line) or \"\" if none.
  `symbol-at-point' returns the symbol under the cursor, or nil if none.
  `symbol-nearest-point' returns the symbol nearest to the cursor, or nil.
Note that these last two functions return symbols, not strings."
  (save-excursion
    (let (beg)
      (unless (looking-at "\\<") (forward-word -1))
      (setq beg (point))
      (forward-word 1)
      (buffer-substring beg (point)))))

(defun symbol-name-before-point ()
  "Return the name of the symbol at (or before) the cursor, as a string.
If no symbol is found, returns the empty string, \"\".
Note: It is possible for the symbol found to be on a previous line.

Some related functions:
  `symbol-name-nearest-point' returns the name of `symbol-nearest-point'
    as a string, or \"\" if none.
  `word-before-point' returns the word at or before the cursor as a string.
  `symbol-at-point' returns the symbol under the cursor, or nil if none.
  `symbol-nearest-point' returns the symbol nearest to the cursor, or nil.
Note that these last two functions return symbols, not strings."
  (save-excursion
    (if (and (not (looking-at "\\s_\\|\\sw")) ; Not in a symbol and no symbol
             (not (re-search-backward "\\s_\\|\\sw" nil t))) ; in previous sexp
        ""
      (buffer-substring
       (progn (forward-sexp 1)
              (while (not (looking-at "\\s_\\|\\sw")) (backward-char 1))
              (forward-char 1) (point))
       (progn (backward-sexp 1)
              (while (not (looking-at "\\s_\\|\\sw")) (forward-char 1))
              (point))))))

;; Stolen from `sql-mode.el'.
(defun echo-in-buffer (buffer-name string &optional force-display-p)
  "Display string STRING in buffer BUFFER-NAME, creating buffer if needed.
FORCE-DISPLAY-P non-nil means buffer is displayed."
  (let ((buffer (get-buffer-create buffer-name)))
    (when force-display-p (display-buffer buffer))
    ;; There is probably a better way to do this.
    (set-buffer buffer)
    (goto-char (point-max))
    (insert string)
    (when force-display-p
      (set-window-point (get-buffer-window buffer-name) (point-max)))))

(defvar minibuffer-empty-p t "Non-nil iff minibuffer is empty (not guaranteed).
This flag is not guaranteed to represent the state of the minibuffer,
but only the memorized state.  Use the function of the same name to be sure.")

(defun set-minibuffer-empty-p (flag)
  "Set value of variable `set-minibuffer-empty-p' to FLAG."
  (setq minibuffer-empty-p flag))

(defun minibuffer-empty-p ()
  "Return non-nil iff minibuffer is empty.
Sets variable `minibuffer-empty-p' to returned value."
  (save-window-excursion
    (with-current-buffer (window-buffer (minibuffer-window))
      (set-minibuffer-empty-p (= 0 (buffer-size))))))

;;;###autoload
(defun erase-nonempty-inactive-minibuffer ()
  "Erase the minibuffer, if inactive and not `minibuffer-empty-p'.
To do this at each user input event:
   (add-hook 'pre-command-hook 'erase-nonempty-inactive-minibuffer).

Note that `minibuffer-empty-p' is not infallible.  To make sure the
minibuffer is emptied, you can use the surer, though slower, function
`erase-inactive-minibuffer'."
  (interactive) (or minibuffer-empty-p (erase-inactive-minibuffer)))

;;;###autoload
(defun erase-inactive-minibuffer ()
  "Erase the minibuffer (remove its contents), provided it is inactive.
To ensure that the minibuffer is erased at each user input, do this:
   (add-hook 'pre-command-hook 'erase-inactive-minibuffer).
This is generally costly, however.  For a faster, though less sure,
alternative, see `erase-nonempty-inactive-minibuffer'."
  (interactive)
  (let ((win (minibuffer-window)))
    (unless (minibuffer-window-active-p win)
      (message nil)                     ; Clear any messages to show minibuf.
      (save-window-excursion
        (with-current-buffer (window-buffer win)
          (erase-buffer)
          (set-minibuffer-empty-p t)))
      (message nil))))                  ; Clear any messages to show minibuf.

(defun string-w-face (arg)
  "Convert ARG (of form (FACE OBJECT)) to a string with face FACE.
If ARG is already a string, any text (face) properties are preserved.

ARG may be a string or a number (see `insert-string'), or nil
\(ignored).  As a special case, it may also be a list of the form
\(FACE OBJECT), where OBJECT is an object to be converted to a string
via (format \"%s\"), and FACE is the face in which to display the
resulting string.  If OBJECT is a string, any text properties
belonging to it are ignored.

NOTE: For versions of Emacs that do not have faces, a list of
      (FACE OBJECT) is simply treated as the string resulting from
      (format \"%s\" OBJECT)."
  (when (consp arg)
    (let ((strg (format "%s" (cadr arg)))) ; Convert to plain string.
      (when (fboundp 'set-face-foreground) ; E.g. not Emacs 19.
        (setq arg                     ; Error if, e.g., `"' in string.
              (condition-case nil
                  (car (read-from-string
                        (format "#%s"
                                (cons (concat "\"" strg "\"")
                                      (list 0 (length strg)
                                            (list 'face (car arg)))))))
                (error nil))))
      (unless (stringp arg) (setq arg strg)))) ; Use uncolored string.
  arg)

;;;###autoload
(defun concat-w-faces (&rest arguments)
  "Return the string that is the concatenation of all ARGUMENTS.
Text (face) properties of any string arguments are preserved.

This is obsolete.  Use `concat' with `put-text-property' or
`propertize' now.

Items in arg list may be strings or numbers (see `insert-string'), or
nil (ignored).  As a special case, they may also be lists of the form
\(FACE OBJECT), where OBJECT is an object to be converted to a string
via (format \"%s\"), and FACE is the face in which to display the
resulting string.  If OBJECT is a string, any text properties
belonging to it are ignored.

NOTE: For versions of Emacs that do not have faces, a list of
      (FACE OBJECT) is simply treated as the string resulting from
      (format \"%s\" OBJECT)."
  (interactive "sString: ") (mapconcat 'string-w-face arguments ""))

;;;###autoload
(defun insert-in-minibuffer (&rest arguments)
  "Insert ARGUMENTS in minibuffer, indefinitely, preserving faces.
The minibuffer is not erased before display.  If you want to ensure
that the minibuffer is erased at each user input event, then do this:
    (add-hook 'pre-command-hook 'erase-inactive-minibuffer)
or  (add-hook 'pre-command-hook 'erase-nonempty-inactive-minibuffer)

Text (face) properties of string arguments are preserved.

Items in arg list may be strings or numbers (see `insert-string'), or
nil (ignored).  As a special case, they may also be lists of the form
\(FACE OBJECT), where OBJECT is an object to be converted to a string
via (format \"%s\"), and FACE is the face in which to display the
resulting string.  If OBJECT is a string, any text properties
belonging to it are ignored.

NOTE: For versions of Emacs that do not have faces, a list of
      (FACE OBJECT) is simply treated as the string resulting from
      (format \"%s\" OBJECT)."
  (interactive "sString: ")
  (message nil)                         ; Clear any messages to show minibuf.
  (save-excursion
    (save-window-excursion
      (with-current-buffer (window-buffer (minibuffer-window))
        (goto-char (point-max))
        (insert (apply 'concat-w-faces arguments)))))
  (when arguments (set-minibuffer-empty-p nil))
  (message nil) (sit-for 0))            ; Clear any messages & show minibuf.


;; REPLACES ORIGINAL (built-in):
;; 1. Uses `completing-read'.
;; 2. Interactively, uses `another-buffer' or `other-buffer' if no default.
;; 3. Emacs 23 compatible: handles `read-buffer-function'
;;    and `read-buffer-completion-ignore-case'.
;;
(defun read-buffer (prompt &optional default require-match)
  "Read the name of a buffer and return it as a string.
Prompts with first arg, PROMPT (a string).

Non-nil DEFAULT names the default buffer.
Otherwise, `another-buffer' is used as the default.
If `another-buffer' is undefined, then `other-buffer' is the default.

Non-nil REQUIRE-MATCH means to allow only names of existing buffers."
  (if (and (boundp 'read-buffer-function) read-buffer-function)
      (funcall read-buffer-function prompt)
    (when (interactive-p)
      (setq default (or default (if (fboundp 'another-buffer) ; In `misc-fns.el'.
                                    (another-buffer nil t)
                                  (other-buffer (current-buffer))))))
    ;; Need a string as default.
    (when (bufferp default) (setq default (buffer-name default)))
    (let ((completion-ignore-case  (if (boundp 'read-buffer-completion-ignore-case)
                                       read-buffer-completion-ignore-case
                                     completion-ignore-case)))
      (completing-read
       prompt (mapcar (lambda (b) (list (buffer-name b))) (buffer-list))
       nil require-match nil 'minibuffer-history default t))))

(defun buffer-alist (&optional nospacep)
  "Alist of (BUF-NAME . BUF) items, where BUF-NAME (a string) names BUF,
which is in (buffer-list).  Non-nil NOSPACEP means do not include
buffers whose names start with SPACE."
  (let (bn-alist)
    (mapc (lambda (buf) (let ((bn (buffer-name buf)))
                            (unless (and nospacep (equal " " (substring bn 0 1)))
                              (push (cons bn buf) bn-alist))))
            (buffer-list))
    (reverse bn-alist)))

;; Same as Emacs 22 standard definition, except:
;;  1. Allow for `replace-regexp-in-string' not being defined.
;;  2. Allow for error reading input.
;;  3. Call `ding' if not a number, and don't redisplay for `sit-for'.
(unless (fboundp 'read-number)          ; Defined in `subr.el' in Emacs 22+.
  (defun read-number (prompt &optional default)
    "Read a number in the minibuffer, prompting with arg PROMPT.
PROMPT is a string.
DEFAULT is returned if the user hits `RET' without typing anything."
    (let ((num nil))
      (when default
        (setq prompt
              (cond ((string-match "\\(\\):[ \t]*\\'" prompt)
                     (replace-match (format " (default %s)" default) t t prompt 1))
                    ((fboundp 'replace-regexp-in-string)
                     (replace-regexp-in-string "[ \t]*\\'"
                                               (format " (default %s) " default)
                                               prompt t t))
                    (t prompt))))
      (while
          (progn
            (let ((str (read-from-minibuffer
                        prompt nil nil nil nil (and default
                                                    (number-to-string default)))))
              (setq num (cond ((zerop (length str)) default)
                              ((stringp str)
                               (condition-case nil (read str) (error nil))))))
            (unless (numberp num)
              (ding) (message "Not a number.  Try again.") (sit-for 1 nil t)
              t)))
      num)))


;; REPLACES ORIGINAL (built-in):
;; Uses `symbol-nearest-point' and `completing-read' to get default.
;;      `symbol-nearest-point' is defined in `thingatpt+.el'.
;;      `symbol-at-point' is defined in `thingatpt.el'.
(defun read-variable (prompt &optional default-value)
  "Read name of a user variable (an option) and return it as a symbol.
Prompt with string PROMPT.  By default, return DEFAULT-VALUE if
non-nil.  If DEFAULT-VALUE is nil and the nearest symbol to the cursor
is a variable, then return that by default.
A user variable is one for which `user-variable-p' returns non-nil."
  (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                    ((fboundp 'symbol-at-point) (symbol-at-point))
                    (t nil)))
        (enable-recursive-minibuffers t))
    (when (and default-value (symbolp default-value))
      (setq default-value (symbol-name default-value)))
    (intern (completing-read prompt obarray 'user-variable-p t
                             nil 'minibuffer-history
                             (or default-value (and (user-variable-p symb)
                                                    (symbol-name symb)))
                             t))))

(defun read-any-variable (prompt &optional default-value)
  "Read name of a variable and return it as a symbol.
Unlike `read-variable', which reads only user options, this reads the
name of any variable.

Prompts with arg string PROMPT.  By default, return DEFAULT-VALUE if
non-nil.  If DEFAULT-VALUE is nil and the nearest symbol to the cursor
is a variable, then return that by default."
  (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                    ((fboundp 'symbol-at-point) (symbol-at-point))
                    (t nil)))
        (enable-recursive-minibuffers t))
    (when (and default-value (symbolp default-value))
      (setq default-value (symbol-name default-value)))
    (intern (completing-read prompt obarray 'boundp t
                             nil 'minibuffer-history
                             (or default-value (and (boundp symb)
                                                    (symbol-name symb)))
                             t))))

;;; See also `make-frame-names-alist', defined in `frame.el'.
(defun frame-alist ()
  "Alist of (FR-NAME . FR) items.  FR-NAME names FR in `frame-list'.
FR-NAME is a string.  The alist is sorted by ASCII code in reverse
alphabetical order, and with case ignored."
  (sort (mapcar (function (lambda (fr) (cons (get-frame-name fr) fr)))
                (frame-list))
        (function
         (lambda (f1f1n f2f2n)
           (not (string< (downcase (car f1f1n)) (downcase (car f2f2n))))))))


;; This is not so useful anymore.
;; With Emacs 21+ you can instead use `message' with face-property strings.
;;
;;;###autoload
(defun display-in-minibuffer (option &rest arguments)
  "Display ARGUMENTS in minibuffer, preserving their face properties.
This function essentially allows you to display messages with faces.

First arg OPTION determines the display behavior, as follows:

 OPTION values `event', `new', and a `natnump' erase the minibuffer
 before displaying.  Other values do not.  They are intended for later
 use to add to the contents of the minibuffer.

 OPTION values `event', `more-event' and an `integerp' are guaranteed
 to erase the minibuffer at some time after displaying.  Other values
 do not erase it afterward.  They allow you to later add more to the
 current contents of the minibuffer.  Remember that they leave the
 minibuffer with text in it.  They should therefore at some point be
 followed by something that erases the contents, such as
 `erase-inactive-minibuffer'.

 OPTION values `event' and a `natnump' are common, one-shot affairs.
 The other values are only used when you need to combine several
 submessages via separate calls.

 OPTION values `event' and `more-event' block Emacs execution until
 the next user event.  This means, among other things, that such a
 call should always be the last of a sequence of calls to
 `display-in-minibuffer'.

 Here are all the OPTION values:

 If a number: ARGS are displayed for that many seconds (`sit-for'),
      then the minibuffer is erased.  The minibuffer is also
      erased before the display of ARGS, iff the number is >= 0.
 If `event': ARGS are displayed until the next user event, after
      erasing the minibuffer.  (If ARGS = nil, this just affects
      the duration of the current minibuffer contents.)
 If `more-event': ARGS displayed until next user event, without
      first erasing minibuffer.  (If ARGS = nil, this just affects
      the duration of the current minibuffer contents.)
 If `new': ARGS displayed indefinitely, after erasing minibuffer.
      (If ARGS = nil, then this just erases the minibuffer.)
 Else (e.g. `more'): ARGS displayed indefinitely, without first
      erasing minibuffer.  (If ARGS = nil, then this is a no-op.)

If you cannot (or do not want to) explicitly program the ultimate
erasure of the minibuffer, and yet you do not want to block program
execution by waiting for a time elapse or a user input, you may still
ensure that the minibuffer is always erased at the next user input,
by doing either of these (the first is surer, but slower):
    (add-hook 'pre-command-hook 'erase-inactive-minibuffer)
or  (add-hook 'pre-command-hook 'erase-nonempty-inactive-minibuffer)

This can be a good thing to do (but it can also slow things down
considerably).  You may then freely use OPTION values other than
numbers, `event' and `more-event' (e.g.  `new' and `more'), without
fear of indefinite display.  However, user input between `new' and
`more' will then interfere with incremental display.  If you do arm
`pre-command-hook' this way, you can always inhibit erasure
temporarily by rebinding `pre-command-hook' to nil.


The remaining arguments, besides OPTION, may be strings or numbers
\(see `insert-string'), or nil (ignored).

As a special case, they (items in the ARGS list) may also be lists of
the form (FACE STRING), where STRING is a string and FACE is the face
in which it is to be displayed.  In this case, any text properties
belonging to STRING itself are ignored.


EXAMPLE (one-shot, without `sit-for', erasure upon user event):
  (display-in-minibuffer 'event \"a\" \"b\") ; Erase, then display until event.
  ...                                        ;  --> ab

EXAMPLE (multiple calls, without `sit-for', erasure upon user event):
  (display-in-minibuffer 'new \"a\" \"b\")  ; Erase, then display.
  ...                                       ;  --> ab
  (display-in-minibuffer 'more \"cd\")      ; Display (no erase).
  ...                                       ;  --> abcd
  (display-in-minibuffer 'more-event \"ef\"); Display until user event.
  ...                                       ;  --> abcdef

EXAMPLE (without `sit-for', explicit erasure later):

  (display-in-minibuffer 'new \"ab\")     ; Erase, then display.
  ...                                     ;  --> ab
  (display-in-minibuffer 'more \"cd\")    ; Display (no erase).
  ...                                     ;  --> abcd
  (display-in-minibuffer 'new)            ; Erase---same as
                                          ; (erase-inactive-minibuffer).
  ...                                     ;  -->

EXAMPLE (with positive `sit-for'):
  (display-in-minibuffer 3 \"abc\" \"def\") ; Erase, display 3 sec, erase.

EXAMPLE (with negative `sit-for'):
  (display-in-minibuffer new \"abc\" \"def\") ; Erase, then display.
  ...
  (display-in-minibuffer -3 \"gh\")         ; Display (\"abcdefgh\") 3 sec.


NOTE:
 This function is not very useful interactively, especially as regards
 different values of OPTION.  Interactive calls in fact always erase
 the minibuffer first.
 Regardless of this, if interactive, then OPTION is the numeric value
 of the prefix arg, if any.  If there is no prefix arg, behavior is as
 if OPTION were `event': display contents until the next user event."
  (interactive "P\nsString: ")
  (when (interactive-p)
    (if option
        (setq option (prefix-numeric-value option))
      (setq option 'event)))
  (when (or (natnump option) (memq option '(event new)))
    (erase-inactive-minibuffer))
  (apply (function insert-in-minibuffer) arguments)
  (cond ((integerp option) (sit-for (abs option)) (erase-inactive-minibuffer))
        ((memq option '(event more-event))
         (while (not (input-pending-p)) (sit-for 0))
         (erase-inactive-minibuffer))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'strings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; strings.el ends here
