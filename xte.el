;;
;; xte.el;; Copyright (C) 2004-2007 by Stefan Reichoer

;; Emacs Lisp Archive Entry
;; Filename: xte.el
;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; xte.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; xte.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary
;; The latest version of xte.el can be found at:
;;   http://www.xsteve.at/prg/emacs/xte.el

;; You need the xautomation tools from http://hoopajoo.net/projects/xautomation.html
;; On a debian/ubuntu system just use: apt-get install xautomation

;; xte.el allows to generate fake keyboard input via the xte commad line tool
;; The main entry point is the function xte.

;; To insert "hello" on the beginning of the current line, do
;; M-: (xte [(control ?a) "hello"])

;; I use xte in conjunction with ion3 to send input to other applications

;; See xte-key-symbol-mapping for some special keys and their names
;; Strings can be defined by using "", for example "hello emacs!"

;; The modifier key meta, control and hyper are supported

;; xte.el uses xautomation and runs therefore on X window systems
;; sww.el + sww.exe (also from me) provides a similar functionality for windows systems

;; Comments / suggestions welcome!

(defvar xte-show-debug-messages nil)
;;(setq xte-show-debug-messages t)

(defvar xte-clear-modifiers-before-sending t)

(defvar xte-key-symbol-mapping
  '((home . "Home")
    (left . "Left")
    (up . "Up")
    (right . "Right")
    (down . "Down")
    (prior . "Page_Up")
    (next . "Page_Down")
    (end . "End")
    (return . "Return")
    (backspace . "Backspace")
    (tab . "Tab")
    (escape . "Escape")
    (delete . "Delete")))

;;; Code:
(defun xte (arg)
  (interactive "sxte: ")
  (when (vectorp arg)
    (setq arg (xte-parse-keys arg)))
  (when xte-show-debug-messages
    (message "xte: %S" arg))
  (when xte-clear-modifiers-before-sending
    (xte-clear-modifier-keys))
  (xte-run arg))

(defun xte-run (command-string)
  (shell-command-to-string (concat "xte " command-string)))

(defun xte-clear-modifier-keys ()
  "Clear the currently pressed modifier strings. It would be better to wait
until no modifier keys are pressed. But I don't know if this is possible via emacs."
  (let* ((cur-keys (this-single-command-keys))
         (modifiers (event-modifiers (aref cur-keys (- (length cur-keys) 1)))))
    ;;(message "modifiers: %S" modifiers)
    (when (member 'meta modifiers)
      (when xte-show-debug-messages
        (message "clearing meta flag"))
      (xte-run "\"keyup Meta_L\""))
    (when (member 'control modifiers)
      (when xte-show-debug-messages
        (message "clearing control flags"))
      (xte-run "\"keyup Control_L\" \"keyup Control_R\""))))

(defun xte-parse-keys (keys)
  (let ((total-key-string "")
        (key-string ""))
    (cond ((vectorp keys)
           (dolist (keyseq (append keys nil))
             (cond ((listp keyseq)
                    (let ((has-meta)
                          (has-control)
                          (has-hyper))
                      (dolist (key keyseq)
                        (cond ((eq key 'meta)
                               (setq has-meta t))
                              ((eq key 'control)
                               (setq has-control t))
                              ((eq key 'hyper)
                               (setq has-hyper t))
                              (t
                               ;(message "seq %S" key)
                               (setq key-string (concat key-string (xte-keysym key))))))
                      (when has-hyper
                        (setq key-string (concat "\"keydown Hyper_R\" \"usleep 1\" "
                                                 key-string "\"usleep 1\" \"keyup Hyper_R\" \"usleep 1\" ")))
                      (when has-meta
                        (setq key-string (concat "\"keydown Meta_L\" \"usleep 1\" "
                                                 key-string "\"usleep 1\" \"keyup Meta_L\" ")))
                      (when has-control
                        (setq key-string (concat "\"keydown Control_L\" \"usleep 1\" "
                                                 key-string "\"usleep 1\" \"keyup Control_L\" \"usleep 1\" ")))))
                   ((stringp keyseq)
                    (setq key-string (format "\"str %s\" " keyseq)))
                   (t
                    (let ((key-symbol
                           (cond ((characterp keyseq) (char-to-string keyseq))
                                 (t
                                  (cdr (assoc keyseq xte-key-symbol-mapping)))
                                 )))
                      (if (eq keyseq 'sleep)
                          (setq key-string "\"usleep 5\" ")
                        (setq key-string (concat "\"key " key-symbol "\" \"usleep 1\" "))))))
             ;;(message "single %S %s" keyseq key-string)))
             (setq total-key-string (concat total-key-string key-string))))
          ((stringp keys)
           (message "string")))
    ;;(message "afd %S" total-key-string)
    total-key-string))

(defun xte-keysym (key)
  (format "\"key %c\" " key))

;; --------------------------------------------------------------------------------
;; A bunch of examples

;;(xte-parse-keys [(meta ?a) left])

;;(xte-parse-keys [(meta ?h)])
;;(xte [(control ?a)])

;;(xte-parse-keys [(hyper ?k) ?k prior sleep (hyper ?k) sleep ?k sleep])
;;(xte-parse-keys [(hyper ?k) ?k prior (hyper ?k) ?k])
;;(xte-parse-keys [(hyper ?k) ?k])
;;(xte-parse-keys [?k])
;;(xte-parse-keys [prior next left])
;;(xte-parse-keys [(hyper ?k) ?k prior sleep (hyper ?k) ?k])

;;
;;(defun xte-recent-win-page-up ()
;;  (interactive)
;;  (xte [(hyper ?k) ?k prior sleep (hyper ?k) ?k]))
;;
;;(defun xte-recent-win-page-down ()
;;  (interactive)
;;  (xte [(hyper ?k) ?k next sleep (hyper ?k) ?k]))

; (global-set-key [kp-subtract] 'xte-recent-win-page-up)
; (global-set-key [kp-add] 'xte-recent-win-page-down)

(provide 'xte)

;; arch-tag: afc6ee50-5fca-4605-910f-24c19f54b5be
