;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mouse-embrace.el --- Minor Mode to embrace text with mouse selections

;; Copyright (C) 2001-2004 by Stefan Reichoer

;; Emacs Lisp Archive Entry
;; Filename: mouse-embrace.el
;; Author: Stefan Reichoer, <xsteve@nit.at>
;; Version: 0.61

;; $Id: mouse-embrace.el 65 2004-02-19 12:52:48Z reichr $

;; mouse-embrace.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; mouse-embrace.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; mouse-embrace.el is tested with GNU Emacs 20.7.1 and with GNU Emacs 21.3

;; This minor mode enables you to select a word or a region of text
;; (e.g. with a mouse double-click). After you have finished the text
;; selection some (user definable) text is put in front of the
;; selection and some text is put after the end of the selection.

;; M-x mouse-embrace-mode toggles the embrace minor mode


;; To use mouse-embrace.el put the following lines in your .emacs:

;; (require 'mouse-embrace)
;; (global-set-key [(control c) (e)] 'mouse-embrace-mode) ;control-c e
;; (define-key mouse-embrace-mode-map [(S-down-mouse-1)] 'mouse-embrace-choose-text)

;; You can make the variables mouse-embrace-pair and mouse-embrace-choices
;; buffer local, e.g.:

;; mouse-embrace-pair: ("\index{" "}")
;; mouse-embrace-choices: (("\index{" "}") ("$"  "$"))


;; The latest version of mouse-embrace.el can be found at:
;;   http://xsteve.nit.at/prg/emacs/mouse-embrace.el


;; Comments / suggestions welcome!

;;; Code:

(defvar mouse-embrace-pair '("$" "$") "Start string and End string for mouse-embracing")
(make-local-variable 'mouse-embrace-pair)

(defvar mouse-embrace-choices '(("[+" "]") ("begin "  " end") ("$" "$") ("\index{" "}"))
  "List of choices for mouse-embrace")
(make-local-variable 'mouse-embrace-choices)

;;; End of user settings
(require 'easy-mmode)

(defun mouse-embracer ()
  "Embrace the selected text with mouse-embrace-pair"
  (when (and mouse-embrace-mode (mouse-region-match))
    (insert (cadr mouse-embrace-pair))
    (save-excursion
      (goto-char (region-beginning))
      (insert (car mouse-embrace-pair)))))

(unless (boundp 'mouse-embrace-mode-map)
  (setq mouse-embrace-mode-map (make-sparse-keymap)))

;define mouse-embrace-mode
(easy-mmode-define-minor-mode mouse-embrace-mode
   "Toggle mouse embrace mode"
   nil
   " Embrace"
   mouse-embrace-mode-map)


(defun mouse-embrace-hook-function ()
  (if mouse-embrace-mode
      (add-hook 'post-command-hook 'mouse-embracer)
    (remove-hook 'post-command-hook 'mouse-embracer)))

(add-hook 'mouse-embrace-mode-hook 'mouse-embrace-hook-function)

(defun mouse-embrace-choose-text ()
  "Choose the text that should be used for mouse embracing"
  (interactive)
  (let* ((start-string)
         (end-string)
         (menu-entries
          (mapcar '(lambda (elem)
                     (cons (concat (car elem) " ... " (cadr elem))
                           elem))
                  mouse-embrace-choices))
         (menu (list "Select embrace pair"
                     (cons "Operate" (append (list (cons "Define new entry" 'new-entry))
                                             menu-entries))))
         (choice (x-popup-menu t menu)))
    (cond ((eq choice 'new-entry)
           (setq start-string (read-string "Embrace start: " nil))
           (setq end-string (read-string "Embrace end: " nil))
           (setq mouse-embrace-pair (list start-string end-string))
           (setq mouse-embrace-choices (append (list mouse-embrace-pair)
                                               mouse-embrace-choices)))
          (choice
           (setq mouse-embrace-pair choice)))))


(provide 'mouse-embrace)

;; arch-tag: 0a4c413d-fb97-4e48-b4f7-942b78d0ae5c
