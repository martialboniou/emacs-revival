;;; mwheel-term.el --- Mouse support for MS intelli-mouse type mice

;; Copyright (C) 1998, Free Software Foundation, Inc.
;; Maintainer: William M. Perry <wmperry@cs.indiana.edu>
;; Keywords: mouse

;; This file is part of XEmacs known as mwheel

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary: this version is an old version of mwheel without timer
;;;             it works well in xterm, RXvts and iTerm2.app
;;;             the file is renamed not to confuse with the official mwheel

;;; Code:

(require 'custom)
(require 'cl)

(defconst mwheel-term-running-xemacs (string-match "XEmacs" (emacs-version)))

(defcustom mwheel-term-scroll-amount '(5 . 1)
  "Amount to scroll windows by when spinning the mouse wheel.
This is actually a cons cell, where the first item is the amount to scroll
on a normal wheel event, and the second is the amount to scroll when the
wheel is moved with the shift key depressed.
This should be the number of lines to scroll, or `nil' for near
full screen.
A near full screen is `next-screen-context-lines' less than a full screen."
  :group 'mouse
  :type '(cons
      (choice :tag "Normal"
          (const :tag "Full screen" :value nil)
          (integer :tag "Specific # of lines"))
      (choice :tag "Shifted"
          (const :tag "Full screen" :value nil)
          (integer :tag "Specific # of lines"))))

(defcustom mwheel-term-follow-mouse nil
  "Whether the mouse wheel should scroll the window that the mouse is over.
This can be slightly disconcerting, but some people may prefer it."
  :group 'mouse
  :type 'boolean)

(if (not (fboundp 'event-button))
    (defun mwheel-term-event-button (event)
      (let ((x (symbol-name (event-basic-type event))))
    (if (not (string-match "^mouse-\\([0-9]+\\)" x))
        (error "Not a button event: %S" event))
    (string-to-int (substring x (match-beginning 1) (match-end 1)))))
  (fset 'mwheel-term-event-button 'event-button))

(if (not (fboundp 'event-window))
    (defun mwheel-term-event-window (event)
      (posn-window (event-start event)))
  (fset 'mwheel-term-event-window 'event-window))

(defun mwheel-term-scroll (event)
  (interactive "e")
  (let ((curwin (if mwheel-term-follow-mouse
            (prog1
            (selected-window)
              (select-window (mwheel-term-event-window event)))))
    (amt (if (memq 'shift (event-modifiers event))
         (cdr mwheel-term-scroll-amount)
           (car mwheel-term-scroll-amount))))
    (case (mwheel-term-event-button event)
      (4 (scroll-down amt))
      (5 (scroll-up amt))
      (otherwise (error "Bad binding in mwheel-term-scroll")))
    (if curwin (select-window curwin))))

(define-key global-map (if mwheel-term-running-xemacs 'button4 [mouse-4])
  'mwheel-term-scroll)

(define-key global-map (if mwheel-term-running-xemacs [(shift button4)] [S-mouse-4])
  'mwheel-term-scroll)

(define-key global-map (if mwheel-term-running-xemacs 'button5 [mouse-5])
  'mwheel-term-scroll)

(define-key global-map (if mwheel-term-running-xemacs [(shift button5)] [S-mouse-5])
  'mwheel-term-scroll)

(provide 'mwheel-term)
