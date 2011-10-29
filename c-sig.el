;; c-sig.el		signature tool for news and mail

;; Copyright (C) 1995-1999 Free Software Foundation, Inc.

;; Author: Ken Shibata <kshibata@tky.3web.ne.jp>
;; Maintainer: Ken Shibata <kshibata@tky.3web.ne.jp>
;; Created: Sep 1995 - first release to internet
;; Modified: Jun 1999 - wrote English documents
;; Version: $Id: c-sig.el,v 3.8 1999/06/10 00:00:00 kshibata Exp kshibata $
;; Keywords: mail news signature

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'c-sig)
(require 'mail-utils)

(defconst c-sig-version-number "3.8")

(defun c-sig-version ()
  (interactive)
  (message "Using c-sig version %s" c-sig-version-number))


(defvar sig-replace-string nil
  "non-nil, replace strings in signature.")

(defvar sig-insert-end nil
  "non-nil, insert signature at the of the mail.\n\
nil, insert signature before current line.")

(defvar sig-purge t
  "if non-nil, purge void line at the end of mail,\n\
and if the value is string, insert it there.")

(defvar sig-separator nil
  "if string is set, insert it before signature.")

(defvar sig-save-to-sig-name-alist t
  "if non-nil, activate sig-name-alist")

(defvar sig-default-name nil
  "default signature name")

(defvar sig-save-file-every-change t
  "if non-nil, save sig-alist-file every time you made changes.
if nil, save sig-alist-file in kill-emacs.")

(defvar sig-make-backup-files t
  "if t, make backup file for sig-alist-file")

(defvar sig-end-of-headers "^$\\|^--"
  "Regular expression to look for the end of headers")

(defvar sig-search-functions (list 'sig-search-name-alist 'sig-search-regexp)
  "")

(defvar sig-random-sig-list nil
  "list of random signature")

(defvar sig-replace-string-file (expand-file-name "~/.signature.replace")
  "File name for replace strings")

(defvar sig-alist-file (expand-file-name "~/.signature.alist")
  "File name for alists")

(defvar c-sig-load-hook nil
  "User definable hook. Runs after c-sig is loaded.")

(defvar sig-buffer-name "*sig-buffer*")
(defvar sig-editor-name "*sig-editor*")
(defvar sig-electric-mode-map nil
  "*Keymap for sig-electric-mode.")
(defvar sig-alist nil)
(defvar sig-name-alist nil)
(defvar sig-regexp-alist nil)

(setq sig-need-to-save nil)
(setq sig-delete-mode nil)
(setq sig-normal-mode t)

(if (file-exists-p sig-alist-file)
    (load sig-alist-file))

(if (not sig-save-file-every-change)
    (progn
      (if (not (fboundp 'c-sig-orig:kill-emacs))
	  (fset 'c-sig-orig:kill-emacs (symbol-function 'kill-emacs)))
      (defun c-sig:kill-emacs (&optional query)
	(interactive "P")
	(write-sig-file)
	(c-sig-orig:kill-emacs query))
      (fset 'kill-emacs
	    (symbol-function 'c-sig:kill-emacs))))

(if sig-electric-mode-map
    nil
  (setq sig-electric-mode-map (make-sparse-keymap))
  (define-key sig-electric-mode-map "p"     'sig-eref-prev)
  (define-key sig-electric-mode-map "P"     'sig-eref-prev)
  (define-key sig-electric-mode-map "n"     'sig-eref-next)
  (define-key sig-electric-mode-map "N"     'sig-eref-next)
  (define-key sig-electric-mode-map "q"     'sig-eref-abort)
  (define-key sig-electric-mode-map "Q"     'sig-eref-abort)
  (define-key sig-electric-mode-map "\r"    'sig-eref-exit)
  (define-key sig-electric-mode-map "\n"    'sig-eref-exit)
  (define-key sig-electric-mode-map "x"     'sig-eref-exit)
  (define-key sig-electric-mode-map "X"     'sig-eref-exit)
  )

(defvar sig-msg1 "Name of Signature: ")
(defvar sig-msg2 "Need to specify a name of the signature")
(defvar sig-msg3 "The signature name is already exists. Do you want to override it?")
(defvar sig-msg4 "Saving...")
(defvar sig-msg5 "Done")
(defvar sig-msg6 "No signature is registered.")
(defvar sig-msg7 "Are you sure ? ")
(defvar sig-msg8 "Regiser this signature for %s? ")

(defun insert-signature-eref (&optional arg)
  "Insert signature from signature alist.
If optinal argument ARG is non-nil, move selected signature
to the head of sig-alist."
  (interactive "P")
  (if sig-alist
      (let* ((sig-def-name "")
	     (sig-key nil)
	     (sig nil))
	(if (setq sig (sig-electric-mode
		       (setq sig-def-name
			     (sig-search-default-signature))))
	    (progn
	      (sig-insert-sig-internal (cdr sig))
	      (if arg
		  (progn
		    (asort 'sig-alist (car sig))
		    (setq sig-need-to-save t)))
	      (if (and sig-save-to-sig-name-alist
		       sig-key
		       (not (string= sig-def-name (car sig)))
		       (y-or-n-p (format sig-msg8 sig-key)))
		  (progn
		    (setq sig-need-to-save t)
		    (aput 'sig-name-alist sig-key (car sig))))
	      (if sig-save-file-every-change
		  (write-sig-file)))))))

(defun insert-signature-randomly ()
  "Insert signature from signature alist randomly."
  (interactive)
  (if sig-alist
      (sig-insert-sig-internal
       (cdr (assoc (sig-get-random-signature) sig-alist )))
    (message sig-msg6)))

(defun insert-signature-automatically ()
  "Insert signature automatically."
  (interactive)
  (let ((sig-key nil))
    (if sig-alist
	(let ((sig-name (sig-search-default-signature)))
	  (sig-insert-sig-internal (if sig-name
				       (cdr (assoc sig-name sig-alist))
				     "")))
      (message sig-msg6))))

(defun sig-insert-sig-internal (sig)
  (if sig
      (save-excursion
	(if sig-purge
	    (sig-purge-void-lines))
	(if (string= sig "")
	    nil
	  (if sig-normal-mode
	      (progn
		(if sig-replace-string
		    (setq sig (sig-replace-string-function sig)))
		(if (fboundp 'sig-filter-function)
		    (setq sig (sig-filter-function sig)))))
	  (if sig-insert-end
	      (goto-char (point-max))
	    (beginning-of-line))
	  (if sig-separator
	      (insert sig-separator))
	  (insert sig))
	(set-buffer-modified-p (buffer-modified-p)))))
  
(defun add-signature ()
  "add new signature into a signature alist."
  (interactive)
  (save-excursion
    (save-window-excursion
      (delete-other-windows)
      (pop-to-buffer sig-editor-name)
      (kill-all-local-variables)
      (local-set-key "\C-c\C-c" 'save-and-exit-signature)
      (local-set-key "\C-c\C-s" 'save-signature)
      (local-set-key "\C-c\C-i" (function
				 (lambda ()
				   (interactive)
				   (let ((sig-separator nil)
					 (sig-save-to-sig-name-alist nil)
					 (sig-normal-mode nil)
					 (sig-purge nil))
				     (insert-signature-eref)))))
      (local-set-key "\C-c\C-q" 'quit-signature)
      (local-set-key "\C-x\C-s" 'save-signature)
      (local-set-key "\C-xk" 'quit-signature)
      (recursive-edit)
      (kill-buffer sig-editor-name))))

(defun delete-signature ()
  "delete signature from signature alist."
  (interactive)
  (if sig-alist
      (let* ((sig (car (sig-electric-mode nil t))))
	(if sig
	    (progn
	      (save-excursion
		(adelete 'sig-alist sig)
		(setq sig-need-to-save t)
		(if sig-save-file-every-change
		    (write-sig-file))
		(message sig-msg5)))))
    (message sig-msg6)))

(defun sig-purge-void-lines ()
  "purge void line at the end of mail."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((cur-pos (point)))
	(if (and (bolp)
		 (eolp)
		 (progn	
		   (while (eq (following-char) ?\n)
		     (forward-char))
		   (eobp)))
	    (delete-region cur-pos (point-max)))
	(goto-char (point-max))
	(if (bolp)
	    (progn
	      (backward-char)
	      (while (eq (preceding-char) ?\n)
		(delete-backward-char 1)))
	  (insert "\n"))
	(if (stringp sig-purge)
	    (progn
	      (goto-char (point-max))
	      (insert sig-purge)))))))


(defun read-sig-file ()
  "read signature database"
  (interactive)
  (load sig-alist-file)
  (setq sig-need-to-save nil))

(defun write-sig-file ()
  "write signature database"
  (interactive)
  (if sig-need-to-save
      (let ((make-backup-files sig-make-backup-files)
	    (version-control nil))
	(message sig-msg4)
	(set-buffer (get-buffer-create " *sig-alist*"))
	(erase-buffer)
;;;
;;; sig-alist
;;;
	(if sig-alist
	    (progn
	      (insert "(setq sig-alist '(\n")
	      (mapcar '(lambda (element)
			 (insert "( "
				 (prin1-to-string (car element))
				 " .\n"
				 (prin1-to-string (cdr element))
				 ")\n"))
		      sig-alist)
	      (insert "))\n\n"))
	  (insert "(setq sig-alist nil)\n\n"))
;;;
;;; sig-name-alist
;;;
	(if sig-name-alist
	    (progn
	      (insert "(setq sig-name-alist '(\n")
	      (mapcar '(lambda (element)
			 (insert (prin1-to-string element) "\n"))
		      sig-name-alist)
	      (insert "))\n"))
	    (insert "(setq sig-name-alist nil)\n"))
;;;
;;; sig-regexp-alist
;;;
	(if sig-regexp-alist
	    (progn
	      (insert "(setq sig-regexp-alist '(\n")
	      (mapcar '(lambda (element)
			 (insert "(" (prin1-to-string (car element)) "\n")
			 (mapcar '(lambda (element2)
				    (insert "\t" (prin1-to-string element2)
					      "\n"))
				 (cdr element))
			 (insert ")\n"))
		      sig-regexp-alist)
	      (insert "))\n"))
	    (insert "(setq sig-regexp-alist nil)\n"))
;;;
;;; file close
;;;
	(write-file sig-alist-file)
	(kill-buffer (current-buffer))
	(setq sig-need-to-save nil)
	(message sig-msg5))))

(defun sig-electric-mode (begin &optional arg)
  (let* ((signature nil)
	 (sig-cur-num 0)
	 (sig-max-num (length sig-alist))
	 (work sig-alist))
    (if (and begin (cdr (assoc begin sig-alist)))
	(while (not (string= (car (car work)) begin))
	  (setq sig-cur-num (1+ sig-cur-num))
	  (setq work (cdr work))))
    (save-excursion
      (save-window-excursion
	(get-buffer-create sig-buffer-name)
	(pop-to-buffer sig-buffer-name)
	(kill-all-local-variables)
	(setq mode-name "Sig"
	      major-mode 'sig-electric-mode)
	(use-local-map sig-electric-mode-map)
	(setq sig-delete-mode arg)
	(set-buffer sig-buffer-name)
	(setq buffer-read-only t)
	(sig-eref-show)
	(recursive-edit)
	(kill-buffer sig-buffer-name)))
    signature))

(defun sig-eref-show (&optional arg)
  "Show reference INDEX in sc-rewrite-header-list."
  (save-excursion
    (set-buffer sig-buffer-name)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (goto-char (point-min))
      (insert (cdr (setq signature (nth sig-cur-num sig-alist))))
      (setq mode-line-process (concat " : " (car signature))))))

(defun sig-eref-next ()
  "Display next reference in other buffer."
  (interactive)
  (if (eq sig-max-num (setq sig-cur-num (1+ sig-cur-num)))
      (setq sig-cur-num 0))
  (sig-eref-show))

(defun sig-eref-prev ()
  "Display previous reference in other buffer."
  (interactive)
  (setq sig-cur-num (if (eq sig-cur-num 0)
			(1- sig-max-num)
		      (1- sig-cur-num)))
  (sig-eref-show))

(defun sig-eref-abort ()
  "Exit from electric reference mode without inserting reference."
  (interactive)
  (setq signature nil)
  (exit-recursive-edit))

(defun sig-eref-exit ()
  "Exit from electric reference mode and insert selected reference."
  (interactive)
  (if (and sig-delete-mode (not (y-or-n-p sig-msg7)))
      (message "")
    (exit-recursive-edit)))

(defun save-and-exit-signature ()
  ""
  (interactive)
  (save-signature)
  (exit-recursive-edit))

(defun quit-signature ()
  ""
  (interactive)
  (exit-recursive-edit))

(defun save-signature ()
  ""
  (interactive)
  (let* ((sig-name (read-input sig-msg1 "")))
    (if (string= sig-name "")
	(error sig-msg2)
      (if (and (cdr (assoc sig-name sig-alist))
	       (not (y-or-n-p sig-msg3)))
	  nil
	(aput 'sig-alist sig-name
	      (buffer-substring (point-min) (point-max)))
	(setq sig-need-to-save t)
	(if sig-save-file-every-change
	    (write-sig-file))))))

;;;
;;; functions for looking for default signature.
;;;
(defun sig-search-default-signature ()
  (let ((ret nil)
	(functions sig-search-functions))
    (while functions
      (if (setq ret (funcall (car functions)))
	  (setq functions nil)
	(setq functions (cdr functions))))
    (or ret sig-default-name)))

(defun sig-search-name-alist ()
  ""
  (let (key keys name pos work)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
 	(if (not (re-search-forward sig-end-of-headers nil t))
	    (goto-char (point-max)))
	(beginning-of-line)
	(narrow-to-region (point-min) (point))
	(setq keys (mail-strip-quoted-names
		    (or (mail-fetch-field "to")
			(mail-fetch-field "newsgroups")
			"")))))
    (while (not (string= keys ""))
      (setq pos (string-match "[ \t\n]*,[ \t\n]*" keys))
      (setq key (substring keys 0 pos))
      (setq keys (if pos (substring keys (match-end 0)) ""))
      (if (setq name (cdr (assoc key sig-name-alist)))
	  (setq sig-key key
		keys "")
	(or sig-key (setq sig-key key))))
    name))

(defun sig-search-regexp ()
  (if sig-regexp-alist
      (let ((w-alist sig-regexp-alist)
	    keys key pos
	    (ret nil))
	(while w-alist
	  (setq keys (mail-strip-quoted-names
		      (mail-fetch-field (car (car w-alist)))))
	  (while keys
	    (setq pos (string-match "[ \t\n]*,[ \t\n]*" keys))
	    (setq key (substring keys 0 pos))
	    (setq keys (if pos (substring keys (match-end 0)) nil))
	    (let ((reg-alist (cdr (car w-alist))))
	      (while reg-alist
		(if (string-match (car (car reg-alist)) key)
		    (setq ret (cdr (car reg-alist))
			  sig-key (mail-strip-quoted-names key)
			  reg-alist nil
			  w-alist nil
			  keys nil)
		  (setq reg-alist (cdr reg-alist))))))
	  (setq w-alist (cdr w-alist)))
	ret)
    nil))

(defun sig-get-random-signature ()
  ""
      (let ((max)
	    (num (random)))
	(if sig-random-sig-list
	    (progn
	      (setq max (length sig-random-sig-list))
	      (setq num (% (if (< num 0) (- num) num) max))
	      (nth num sig-random-sig-list))
	  (setq max (length sig-alist))
	  (setq num (% (if (< num 0) (- num) num) max))
	  (car (nth num sig-alist)))))

(defun sig-replace-string-function (sig)
  ""
  (save-excursion
    (let ((sig-replace-list)
	  (work))
      (if (file-exists-p sig-replace-string-file)
	  (progn
	    (load sig-replace-string-file)
	    (set-buffer (get-buffer-create " *sig-temp*"))
	    (erase-buffer)
	    (insert sig)
	    (while sig-replace-list
	    (goto-char (point-min))
	    (setq work (car (cdr (car sig-replace-list))))
	    (replace-string
	     (car (car sig-replace-list))
;;;;;;
	     (cond
	      ((listp work) (nth (random (length work)) work))
	      ((fboundp work) (funcall work))
	      (t "")))
;;;;;;
	    (setq sig-replace-list (cdr sig-replace-list)))
	    (setq sig (buffer-substring (point-min) (point-max)))
	    (kill-buffer (current-buffer)))))
    sig))


;;; Following functions are taken from sc-alist.el Version 1.0.
;;; sc-alist.el is not included in SuperCite versions 3.X any more.

(defun asort (alist-symbol key)
  "Move a specified key-value pair to the head of an alist.
The alist is referenced by ALIST-SYMBOL. Key-value pair to move to
head is one matching KEY.  Returns the sorted list and doesn't affect
the order of any other key-value pair.  Side effect sets alist to new
sorted list."
  (set alist-symbol
       (sort (copy-alist (eval alist-symbol))
	     (function (lambda (a b) (equal (car a) key))))))


(defun aelement (key value)
  "Makes a list of a cons cell containing car of KEY and cdr of VALUE.
The returned list is suitable as an element of an alist."
  (list (cons key value)))


(defun aheadsym (alist)
  "Return the key symbol at the head of ALIST."
  (car (car alist)))


(defun anot-head-p (alist key)
  "Find out if a specified key-value pair is not at the head of an alist.
The alist to check is specified by ALIST and the key-value pair is the
one matching the supplied KEY.  Returns nil if ALIST is nil, or if
key-value pair is at the head of the alist.  Returns t if key-value
pair is not at the head of alist.  ALIST is not altered."
  (not (equal (aheadsym alist) key)))


(defun aput (alist-symbol key &optional value)
  "Inserts a key-value pair into an alist.
The alist is referenced by ALIST-SYMBOL. The key-value pair is made
from KEY and optionally, VALUE. Returns the altered alist or nil if
ALIST is nil.

If the key-value pair referenced by KEY can be found in the alist, and
VALUE is supplied non-nil, then the value of KEY will be set to VALUE.
If VALUE is not supplied, or is nil, the key-value pair will not be
modified, but will be moved to the head of the alist. If the key-value
pair cannot be found in the alist, it will be inserted into the head
of the alist (with value nil if VALUE is nil or not supplied)."
  (let ((elem (aelement key value))
	alist)
    (asort alist-symbol key)
    (setq alist (eval alist-symbol))
    (cond ((null alist) (set alist-symbol elem))
	  ((anot-head-p alist key) (set alist-symbol (nconc elem alist)))
	  (value (setcar alist (car elem)))
	  (t alist))))


(defun adelete (alist-symbol key)
  "Delete a key-value pair from the alist.
Alist is referenced by ALIST-SYMBOL and the key-value pair to remove
is pair matching KEY.  Returns the altered alist."
  (asort alist-symbol key)
  (let ((alist (eval alist-symbol)))
    (cond ((null alist) nil)
	  ((anot-head-p alist key) alist)
	  (t (set alist-symbol (cdr alist))))))


(defun aget (alist key &optional keynil-p)
  "Returns the value in ALIST that is associated with KEY.
Optional KEYNIL-P describes what to do if the value associated with
KEY is nil.  If KEYNIL-P is not supplied or is nil, and the value is
nil, then KEY is returned.  If KEYNIL-P is non-nil, then nil would be
returned.

If no key-value pair matching KEY could be found in ALIST, or ALIST is
nil then nil is returned. ALIST is not altered."
  (let ((copy (copy-alist alist)))
    (cond ((null alist) nil)
	  ((progn (asort 'copy key)
		  (anot-head-p copy key)) nil)
	  ((cdr (car copy)))
	  (keynil-p nil)
	  ((car (car copy)))
	  (t nil))))


(defun amake (alist-symbol keylist &optional valuelist)
  "Make an association list.
The association list is attached to the alist referenced by
ALIST-SYMBOL. Each element in the KEYLIST becomes a key and is
associated with the value in VALUELIST with the same index. If
VALUELIST is not supplied or is nil, then each key in KEYLIST is
associated with nil.

KEYLIST and VALUELIST should have the same number of elements, but
this isn't enforced.  If VALUELIST is smaller than KEYLIST, remaining
keys are associated with nil.  If VALUELIST is larger than KEYLIST,
extra values are ignored.  Returns the created alist."
  (let ((keycar (car keylist))
	(keycdr (cdr keylist))
	(valcar (car valuelist))
	(valcdr (cdr valuelist)))
    (cond ((null keycdr)
	   (aput alist-symbol keycar valcar))
	  (t
	   (amake alist-symbol keycdr valcdr)
	   (aput alist-symbol keycar valcar))))
  (eval alist-symbol))

(run-hooks 'c-sig-load-hook)
