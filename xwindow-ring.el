;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xwindow-ring.el --- Keep window configurations in a ring

;; Copyright (C) 2000 - 2004 by Stefan Reichoer

;; Filename: xwindow-ring.el
;; Author: Stefan Reichoer, <xsteve@nit.at>
;; Version: 1.1


;; xwindow-ring.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; xwindow-ring.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; This is yet another package to

;; Place the following in your .emacs:
;; (require 'xwindow-ring)
;; (global-set-key [(control meta up)] 'xwindow-ring-add)
;; (global-set-key [(control meta right)] 'xwindow-ring-next)
;; (global-set-key [(control meta left)] 'xwindow-ring-previous)
;; (global-set-key [(control meta down)] 'xwindow-ring-remove)


;; The latest version of xwindow-ring.el can be found at:
;;   http://xsteve.nit.at/prg/emacs/xwindow-ring.el

;; Comments / suggestions welcome!

;;; Code:

;(make-variable-frame-local 'xwindow-ring-ring)
;(make-variable-frame-local 'xwindow-ring-index)

;(frame-parameters)

(setq xwindow-ring-ring (make-ring 7))
(setq xwindow-ring-index 0)

(defun xwindow-ring-add ()
  (interactive)
  (ring-insert xwindow-ring-ring (list (current-window-configuration) (point-marker)))
  (setq xwindow-ring-index 0)
  (message "Added new window configuration number 0."))

(defun xwindow-ring-next ()
  (interactive)
  (setq xwindow-ring-index
        (ring-plus1 xwindow-ring-index (ring-length xwindow-ring-ring)))
  (xwindow-ring-goto))

(defun xwindow-ring-previous ()
  (interactive)
  (setq xwindow-ring-index
        (ring-minus1 xwindow-ring-index (ring-length xwindow-ring-ring)))
  (xwindow-ring-goto))

(defun xwindow-ring-goto ()
  (interactive)
  (unless (ring-empty-p xwindow-ring-ring)
    (let ((window-config (ring-ref xwindow-ring-ring xwindow-ring-index)))
      (set-window-configuration (car window-config))
      ;(goto-char (cadr window-config))
      (message "Using window configuration number %d." xwindow-ring-index))))

(defun xwindow-ring-remove ()
  (interactive)
  (when (memq last-command
              (list 'xwindow-ring-next 'xwindow-ring-previous 'xwindow-ring-add))
    (ring-remove xwindow-ring-ring xwindow-ring-index)
    (message "Removed window configuration number %d." xwindow-ring-index)
    (setq xwindow-ring-index
          (min xwindow-ring-index (- (ring-length xwindow-ring-ring) 1)))))

(provide 'xwindow-ring)

;; arch-tag: 94d86bab-02f4-46c4-acea-8fd385432e15
