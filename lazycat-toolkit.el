;;; lazycat-toolkit.el --- My toolkit

;; Filename: lazycat-toolkit.el
;; Description: My toolkit
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-01-10 23:18:47
;; Version: 0.1
;; Last-Updated: 2009-01-10 23:18:47
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/lazycat-toolkit.el
;; Keywords: lazycat, toolkit
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `mwe-log-commands' `ecb'
;; `shell-command-extension'
;; `ascii' `window-number' `windows' `color-moccur'
;; `cycle-buffer' `basic-edit-toolkit' `completion-ui'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; I always write some code piece, and those code is miscellaneous.
;; So i clean up those code in this file.
;;
;; I always use those code like toolkit.
;;
;; Hope you like... ;)
;;
;; And have some code is i collections, not my code.
;; I forget author name that write those code.
;; I put it in *My collections*.
;;
;; Thanks all emacser!
;;

;;; Installation:
;;
;; Put lazycat-toolkit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lazycat-toolkit)
;;
;; No need more.

;;; Change log:
;;
;; 2009/01/10
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'mwe-log-commands)
(require 'ecb)
(require 'shell-command-extension)
(require 'ascii)
(require 'window-number)
(require 'windows)
(require 'color-moccur)
(require 'cycle-buffer)
(require 'basic-edit-toolkit)
(require 'completion-ui)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar killed-process-name nil
  "The name of that have killed process.")

(defvar find-file-root-prefix "/sudo:root@localhost:"
  "The prefix of root user use in Emacs.")

(defvar point-stack nil
  "The stack position.")

(defvar startup-open-file-list nil
  "File list that startup open.")

(defvar startup-close-file-list nil
  "Buffer list that startup close.")

(defvar my-name "")
(defvar my-full-name "")
(defvar my-mail "")
(defvar my-homepage "")
(defvar my-irc-nick "")
(defvar my-irc-passwd "")
(defvar my-irc-channel-list '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advices ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defadvice list-load-path-shadows (around hidden-window-if-found-nothing activate)
  "This advice hidden output window if found nothing."
  (let (window-config (current-window-configuration))
    ad-do-it
    (with-current-buffer "*Shadows*"
      (goto-char (point-min))
      (when (search-forward-regexp "^No Emacs Lisp load-path shadowings were found$" nil t)
        (kill-buffer)
        (message "No Emacs Lisp load-path shadowings were found.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Killall ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun killall ()
  "Execute command 'killall'."
  (interactive)
  (let ((process-name killed-process-name))
    (setq process-name (read-string (format "Killall: (%s) " process-name) nil nil process-name))
    (setq killed-process-name process-name)
    (shell-command (format "killall %s" process-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Kill syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kill-syntax-forward+ (&optional arg)
  "Kill ARG set of syntax characters after point.
And if `completion-auto-mode' is active,
use function `completion-delete'."
  (interactive "p")
  (if (member 'auto-completion-mode minor-mode-list)
      (completion-delete 'kill-syntax-forward arg)
    (kill-syntax-forward arg)))

(defun kill-syntax-backward+ (&optional arg)
  "Kill ARG set of syntax characters preceding point."
  (interactive "p")
  (if (member 'auto-completion-mode minor-mode-list)
      (completion-backward-delete 'kill-syntax-forward (- arg))
    (kill-syntax-forward (- arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scroll ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scroll-up-one-line()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun scroll-down-one-line()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun scroll-other-window-up-line ()
  "Scroll other window up one line."
  (interactive)
  (scroll-other-window 1))

(defun scroll-other-window-down-line ()
  "Scroll other window line down."
  (interactive)
  (scroll-other-window-down 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Indent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun refresh-file ()
  "Automatic reload current file."
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (indent-buffer)
         (indent-comment-buffer)
         (save-buffer)
         (byte-compile-file buffer-file-name t))
        ((member major-mode '(lisp-mode c-mode perl-mode))
         (indent-buffer)
         (indent-comment-buffer)
         (save-buffer))
        ((member major-mode '(haskell-mode sh-mode))
         (indent-comment-buffer)
         (save-buffer))
        (t (message "Current mode is not supported, so not reload"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pair move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun go-to-next-pair-right()
  "To right of next match parentheses."
  (interactive)
  (while (not (looking-at "\\(['\">)}]\\|]\\)")) (forward-char 1))
  (forward-char 1))

(defun go-to-next-pair-left()
  "To left of previous match parentheses."
  (interactive)
  (backward-char 1)
  (while (not (looking-at "\\(['\"<({]\\|[[]\\)")) (backward-char 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cycle buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cycle-buffer-in-special-mode (special-mode)
  "Cycle in special mode."
  (setq cycle-buffer-filter nil)
  (setq cycle-buffer-filter (cons '(eq major-mode special-mode) cycle-buffer-filter))
  (cycle-buffer-backward-permissive 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Find file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-file-root (file)
  "Find file with root."
  (interactive "fFind file as sudo: ")
  (find-file (concat find-file-root-prefix file)))

(defun find-file-smb(file)
  "Access file through samba protocol."
  (interactive "fFind file as samba: ")
  (find-file (concat "/smb:" file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Date and time ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %T")))

(defun insert-changelog-date ()
  "Insert changelog date, like yyyy/mm/dd."
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Notes create and search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun notes-search(str)
  "Notes search"
  (interactive
   (list
    (moccur-grep-read-regexp moccur-grep-default-mask)))
  (moccur-grep-find my-notes-directory str))

(defun notes-new (str)
  "Create a new notes."
  (interactive "sNotes name: ")
  (find-file (concat my-notes-directory str ".org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Single functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-and-run-lisp (file)
  "This function open a file at TOP window, and execute `run-lisp' at LOWER window."
  (interactive "FFile: ")
  (delete-other-windows)
  (find-file file)
  (split-window-vertically -20)
  (other-window 1)
  (with-current-buffer (current-buffer)
    (call-interactively 'run-lisp)))

(defun emacs-exit ()
  "Exit emacs."
  (interactive)
  (if (get-buffer "*Group*")
      (gnus-group-exit))
  (newsticker--cache-save)
  (see-you-again))

(defun window-number-jump ()
  "Jump to nth window."
  (interactive)
  (window-number-select (read-number "Window number: ")))

(defun ascii-toggle ()
  "Toggle ascii table"
  (interactive)
  (if ascii-display
      (ascii-off)
    (ascii-on)))

(defun dot-emacs()
  "Open dot emacs file."
  (interactive)
  (find-file "~/.emacs"))

(defun startup-open ()
  "The files that need open when emacs startup."
  (interactive)
  (let* ((file-list startup-open-file-list)
         file-name)
    (dolist (file-name file-list)
      (find-file file-name))))

(defun startup-close ()
  "Close when emacs startup."
  (interactive)
  (dolist (file-name startup-close-file-list)
    (if (get-buffer file-name)
        (kill-buffer file-name))))

(defun elisp-index-search+ ()
  "Look up TOPIC in the indies of the Emacs Lisp Reference Manual."
  (interactive)
  (let ((topic (symbol-name (symbol-at-point))))
    (setq topic (read-string (format "Subject to look up <%s>: " topic) nil nil topic))
    (info "elisp")
    (Info-index topic)))

(defun switch-to-scratch ()
  "Select buffer *scratch* in the current window."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun switch-to-messages ()
  "Select buffer *message* in the current window."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun ielm-toggle ()
  "Toggle ielm buffer."
  (interactive)
  (let ((ielm-buffer-name "*ielm*"))
    (if (get-buffer ielm-buffer-name)
        (if (string-equal ielm-buffer-name (buffer-name))
            (bury-buffer)
          (switch-to-buffer ielm-buffer-name))
      (ielm))))

(defun remove-control-M (&optional quiet)
  "Remove ^M at end of line in the whole buffer.
When QUIET is non-nil, don't show report message."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "$" (point-max) t)
          (incf remove-count)
          (replace-match "" nil nil))
        (or quiet (message (format "%d ^M removed from buffer." remove-count)))))))

(defun colp ()
  "Return t if point is first non-whitespace character of line."
  (let (current-point)
    (setq current-point (point))
    (save-excursion
      (back-to-indentation)
      (equal current-point (point)))))

(defun fileline-to-alist (file)
  "Transform line in special file to element of list.
And return list."
  (let (return-list)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) return-list)
        (forward-line +1))
      (nreverse return-list))))

(defun hibernate-disk ()
  "Hibernate disk, this need you install `hibernate' first.
And make command `sudo hibernate-disk' with alias `sl'."
  (interactive)
  (shell-aliase "sl"))

(defun handler-buffer-exit-close ()
  "Handle buffer close when buffer process is exit.
If you want to some buffer close automatically when its' process is over,
just add this function hook it ."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finish\\|exit\\|broken\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun hide-mouse ()
  "Hide mouse by unclutter.
To use this extensions, you need install unclutter in your system."
  (interactive)
  (shell-command "unclutter -idle 1"))

(defun save-screenshots (name)
  "Save shot full-screen.
To use this function, you need install scrot."
  (interactive "sPicture Name: ")
  (shell-command (format "scrot %s%s.png" my-screenshots-storage-directory name))
  (message (concat "You have save screen as " name ".png at: " my-screenshots-storage-directory)))

(defun ecb-toggle-visible ()
  "Toggle the visibility of the ecb windows.
If ecb is not active start ecb with ecb-activate
If this function is called with a prefix argument: call ecb-deactivate"
  (interactive)
  (if ecb-activated-window-configuration
      (ecb-deactivate)
    (ecb-activate)))

(defun my-doxymacs-return ()
  "Advanced C-m for doxymacs multiline comments.
Inserts `*' at the reigning of the new line if
unless return was pressed outside the comment"
  (interactive)
  (let ((current-point (point))
        is-inside)
    (setq is-inside
          (if (search-backward "*/" nil t)
              ;; there are some comment endings - search forward
              (if (search-forward "/*" current-point t)
                  't
                'nil)
            ;; it's the only comment - search backward
            (goto-char current-point)
            (if (search-backward "/*" nil t)
                't
              'nil)))
    ;; go to last char position
    (goto-char current-point)
    ;; the point is inside some comment, insert `*'
    (if is-inside
        (progn
          (insert "\n*")
          (indent-for-tab-command)
          (insert " "))
      ;; else insert only new-line
      (insert "\n"))))

(defun clean-recentf-history ()
  "Clean recentf history of file assoc."
  (interactive)
  (setq recentf-list '())
  (message "Have clean recentf history."))

(defun open-current-log-keyboard-command ()
  "Open log keyboard command of current buffer."
  (interactive)
  (mwe:log-keyboard-commands)
  (mwe:open-command-log-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; My collections ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Below code is i collections, not my code.
;;; I forget the author name that write those code.
;;; Thanks all emacser! ;)

(defun describe-hash (variable &optional buffer)
  "Display the full documentation of VARIABLE (a symbol).
Returns the documentation as a string, also.
If VARIABLE has a buffer-local value in BUFFER (default to the current buffer),
it is displayed along with the global value."
  (interactive
   (let ((v (variable-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read
                (if (and (symbolp v)
                         (hash-table-p (symbol-value v)))
                    (format
                     "Describe hash-map (default %s): " v)
                  "Describe hash-map: ")
                obarray
                (lambda (atom) (and (boundp atom)
                                    (hash-table-p (symbol-value atom))))
                t nil nil
                (if (hash-table-p v) (symbol-name v))))
     (list (if (equal val "")
               v (intern val)))))
  (with-output-to-temp-buffer (help-buffer)
    (maphash (lambda (key value)
               (pp key)
               (princ " => ")
               (pp value)
               (terpri))
             (symbol-value variable))))

(defun ido-sort-mtime ()
  "Sort ido item by modified time."
  (let (ido-temp-list)
    (setq ido-temp-list
          (sort ido-temp-list
                (lambda (a b)
                  (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                        (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                    (if (= (nth 0 ta) (nth 0 tb))
                        (> (nth 1 ta) (nth 1 tb))
                      (> (nth 0 ta) (nth 0 tb)))))))
    (ido-to-end ;; move . files to end (again)
     (delq nil (mapcar
                (lambda (x) (if (string-equal (substring x 0 1) ".") x))
                ido-temp-list)))))

(defun uniquify-buffer-lines ()
  (while
      (progn
        (goto-char (point-min))
        (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1$" nil t))
    (if (= 0 (length (match-string 1)))
        (replace-match "\\2")
      (replace-match "\\1\n\\2"))))

(defun root-user-p ()
  "Return true if the current user is the superuser, root, with user
  id zero."
  (zerop (user-real-uid)))

(defun match-at-point (regexp)
  "Return the buffer substring around point matching REGEXP.
Look for a match starting at or before point.  Move back a character
at a time while still looking at a match ending at the same point.  If
no match is found at or before point, return the first match after
point, or nil if there is no match in the buffer."
  (let ((backup nil) (start nil) (end nil))
    (save-excursion
      (setq backup
            (or (looking-at regexp)
                (and (re-search-forward regexp nil 'limit)
                     (setq end t)
                     (goto-char (match-beginning 0))
                     nil)
                ;; failed search doesn't change match-data
                (re-search-backward regexp nil t)))
      (if (or backup end) (setq start (match-beginning 0)
                                end (match-end 0)))
      (if backup
          (while (and (not (bobp))
                      (progn (backward-char) t)
                      (looking-at regexp)
                      (= (match-end 0) end))
            (setq start (point)))
        (or (bobp) (re-search-forward regexp nil t))))
    (and start
         (progn (goto-char end) t)
         (buffer-substring start end))))

(defun number-at-point ()
  "Return the number at or before point as an integer."
  (let ((n (match-at-point "[0-9]+")))
    (if n (string-to-number n)
      (error "No number found"))))

(defun lock-screen ()
  "Lock screen using (zone) and xtrlock
calls M-x zone on all frames and runs xtrlock.
To use this extension, you need install xtrlock in your system."
  (interactive)
  (save-excursion
    ;; Don't burn LCD power.
    (shell-command "xset dpms force off && sleep 1")
    ;; Lock screen.
    (set-process-sentinel
     (start-process "xtrlock" nil "xtrlock")
     '(lambda (process event)
        (zone-leave-me-alone)))
    (zone)))

(defun blank-line-p ()
  "Return t if current line is blank line.
Otherwise, return nil."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*\n")))

(defun is-digit (x)
  (cond ((stringp x) (is-digit (string-to-char x)))
        ((integerp x) (and (<= ?0 x) (<= x ?9)))
        (t nil)))

(defun is-letter (x)
  (cond ((stringp x) (is-letter (string-to-char x)))
        ((integerp x) (not (equal (downcase x) (upcase x))))
        (t nil)))

(defun try-require (&rest args)
  "Attempt to load a library or module. Return true if all of the libraries
given as arguments are successfully loaded"
  (if (member nil
              (mapcar (lambda (thing)
                        (condition-case e
                            (if (stringp thing)
                                (load-library thing)
                              (require thing))
                          (file-error () nil)))
                      args))
      nil
    t))

(defun insert-after (list aft-el el)
  "Insert EL after AFT-EL in LIST."
  (push el (cdr (member aft-el list)))
  list)

(defun insert-before (list bef-el el)
  "Insert EL before BEF-EL in LIST."
  (nreverse (insert-after (nreverse list) bef-el el)))

(defun list-set-element (list old-el new-el)
  "Set OLD-EL to NEW-EL in LIST."
  (setcar (member old-el list) new-el)
  list)

(defun list-exchange-els (list el1 el2)
  "Exchange places of EL1 and EL2 in LIST."
  (when (or (null (member el1 list))
            (null (member el2 list)))
    (error "el1 or el2 is not in list.")))

(defun frame-relative-coordinates (&optional position)
  "Return frame-relative coordinates from POSITION."
  (or position (setq position (posn-at-point)))
  (let* ((x-y (posn-x-y position))
         (window (posn-window position))
         (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car edges))
          (+ (cdr x-y) (cadr edges)))))

(provide 'lazycat-toolkit)

;;; lazycat-toolkit.el ends here
