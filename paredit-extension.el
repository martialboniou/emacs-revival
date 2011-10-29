;;; paredit-extension.el --- Simple extension base on paredit.el

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-07-28 16:32:52
;; Version: 0.1
;; Last-Updated: 2008-07-28 16:32:55
;; URL: not distributed yet
;; Keywords: paredit
;; Compatibility: GNU Emacs 23.0.60.1

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

;; Features that might be required by this library:
;;
;;  `paredit' `lazycat-toolkit'
;;

;;; Installation:
;;
;; Copy paredit-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'paredit-extension)
;;
;; No need more

;;; Commentary:
;;
;; This extension is base on paredit.el of Taylor R. Campbell.
;;

;;; Change log:
;;
;; 2008/07/28
;;         First release.
;;

;;; Acknowledgments:
;;
;;      Taylor R. Campbell      <campbell@mumble.net>   for paredit.el
;;

;;; TODO
;;
;; None
;;

;;; Require
(require 'lazycat-toolkit)
(require 'paredit)

;;; Code:

(defun paredit-comment-list ()
  "Comment current list."
  (interactive)
  (save-excursion
    (paredit-comment-list-base)))

(defun paredit-comment-list-and-newline ()
  "Comment current list and newline."
  (interactive)
  (paredit-comment-list-base)
  (newline-and-indent))

(defun paredit-comment-list-base ()
  "Base function that comment current list."
  (if (paredit-in-string-p)
      (goto-char (car (paredit-string-start+end-points))))
  (comment-or-uncomment-region
   (save-excursion
     (backward-up-list)
     (point))
   (progn
     (up-list)
     (point))))

(defun paredit-close-round-and-newline+ ()
  "Make paredit can jump and indent behind close quotation when cursor in quotations."
  (interactive)
  (if (paredit-in-string-p)
      (progn
        (goto-char (1+ (cdr (paredit-string-start+end-points))))
        (newline-and-indent))
    (paredit-close-round-and-newline)))

(defun paredit-forward-sexp-and-newline ()
  "Forward sexp and newline.
Newline current point if current sexp is last one."
  (interactive)
  (let ((original-position (point)))
    (unless
        ;; Forward sexp.
        (ignore-errors
          (forward-sexp 1)
          t)
      ;; Otherwise return original position.
      (goto-char original-position))
    ;; Newline and indent.
    (when (looking-at "\\s-*;.*$")
      (end-of-line))
    (newline-and-indent)))

(defun paredit-move-list-forward ()
  "Move current list across forward."
  (interactive)
  (let (text
        init-point
        excursion)
    ;; remember init position
    (setq init-point (point))
    ;; adjust position when in string
    (if (paredit-in-string-p)
        (goto-char (car (paredit-string-start+end-points))))
    ;; get list
    (setq text (delete-and-extract-region
                (progn
                  (backward-up-list)
                  (setq excursion (- init-point (point)))
                  (point))
                (progn
                  (forward-list)
                  (point))))
    ;; clean whitespace before move
    (if (eolp)
        (delete-chars-hungry-forward)
      (delete-chars-hungry-backward))
    ;; jump to paste position
    (paredit-forward)
    ;; adjust position before paste
    (if (eolp)
        (newline-and-indent)
      (insert " "))
    ;; insert list and resume init position before move
    (insert text)
    (backward-list)
    (forward-char excursion)
    ))

(defun paredit-move-list-backward ()
  "Move current list across backward"
  (interactive)
  (let (text
        init-point
        excursion)
    ;; remember init position
    (setq init-point (point))
    ;; adjust position when in string
    (if (paredit-in-string-p)
        (goto-char (car (paredit-string-start+end-points))))
    ;; get list
    (setq text (delete-and-extract-region
                (progn
                  (backward-up-list)
                  (setq excursion (- init-point (point)))
                  (point))
                (progn
                  (forward-list)
                  (point))))
    ;; clean whitespace before move
    (if (eolp)
        (delete-chars-hungry-backward)
      (delete-chars-hungry-forward))
    ;; jump to paste position
    (paredit-backward)
    ;; adjust position before paste
    (if (colp)
        (progn
          (beginning-of-line)
          (open-line 1)
          (indent-according-to-mode))
      (insert " ")
      (backward-char +1))
    ;; insert list and resume init position before move
    (insert text)
    (backward-list)
    (forward-char excursion)
    ))

(defun paredit-insert-quote (arg)
  "Insert quote."
  (interactive "p")
  (if (or (> arg 1) (paredit-in-comment-p) (paredit-in-string-p))
      (self-insert-command (or arg 1))
    (self-insert-command 1)
    (cond
     ((or
       (eq major-mode 'emacs-lisp-mode)
       (eq major-mode 'lisp-mode)
       (eq major-mode 'scheme-mode)
       (eq major-mode 'lisp-interaction-mode)
       (eq major-mode 'org-mode))
      (progn
        (insert "'")
        (backward-char)))
     ((or
       (eq major-mode 'haskell-mode))
      (progn
        (insert "`")
        (backward-char))))))

(defun paredit-splice-sexp+ (&optional argument)
  "This function extension `paredit-splice-sexp'.
Will delete blank line after execute `paredit-splice-sexp'."
  (interactive "P")
  (paredit-splice-sexp argument)
  (when (blank-line-p)
    (call-interactively 'move-beginning-of-line)
    (kill-line)
    (back-to-indentation)))

(provide 'paredit-extension)

;;; paredit-extension.el ends here

;;; LocalWords:  uncomment
