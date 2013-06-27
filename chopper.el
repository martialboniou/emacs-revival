;;; chopper.el -- Dichotomic window and buffer movement
;;
;;; Copyright (C) 2013 Martial Boniou, (C) 2008 Kyle W T Sherman, (C) 2002 Luke Gorrie
;; Author: Martial Boniou <hondana at gmx dot com>
;; Origin: Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created: 2013-06-27
;; Version:  0.1
;; Keywords: chop split move
;; Website: https://github.com/martialboniou/chopper
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; This code originates from a merge of split-move.el and chop.el:
;; - http://nullman.net/tutorial/emacs-files/.emacs.d/kyle-modules/split-move.el.html
;;   Version 1 (2008-01-30) by Kyle W T Sherman <kylewsherman at gmail dot com>
;; - http://fresh.homeunix.net/%7Eluke/misc/emacs/chop.el Version 1.0,
;;   May 2002 by Luke Gorrie <luke@bluetail.com>
;; TODO: change this
;; `chopper-move' moves up or down a window or a buffer in diminishing chunks to
;; hone in on a location.
;;
;;; Installation:
;;
;; Put `chopper.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (require 'chopper)
;;   (eval-after-load "chopper"
;;     '(progn
;;       (global-set-key [(meta up)] 'chopper-window-move-up)
;;       (global-set-key [(meta down)] 'chopper-window-move-down)
;;       (global-set-key [(shift meta up)] 'chopper-buffer-move-up)
;;       (global-set-key [(shift meta down)] 'chopper-buffer-move-down)))
;;
;;; El-get:
;;
;; Use the following recipe: 

(defvar chopper-lines-number nil
  "Number of lines that the next \"chop\" will contain, as floating-point.
Only meaningful for consecutive chops.")

;;;###autoload
(defun chopper-window-move-up ()
  "Move up half the remaining distance of previous chop move."
  (interactive)
  (chopper-move -1))

;;;###autoload
(defun chopper-window-move-down ()
  "Move down half the remaining distance of previous chop move."
  (interactive)
  (chopper-move 1))

;;;###autoload
(defun chopper-buffer-move-up ()
  "Move up half the remaining distance of previous chop move in the whole
buffer."
  (interactive)
  (chopper-move -1 t))

;;;###autoload
(defun chopper-buffer-move-down ()
  "Move down half the remaining distance of previous chop move in the whole
buffer."
  (interactive)
  (chopper-move 1 t))

(defun chopper-move (direction &optional in-buffer)
  "Move by one 'chop' in DIRECTION in the window or the whole buffer if
IN-BUFFER is T.

DIRECTION is 1 for down and -1 for up.

IN-BUFFER is T or NIL.

If this is the first call `chopper-lines-number' is initialized to:
- the middle of the window in the default case;
- half the distance from the current location to the beginning or
end of the buffer if IN-BUFFER is T."
  (setq this-command (if in-buffer 'chopper-buffer-move 'chopper-window-move))
  (if (chopper-new-p this-command)
      ;; first chop
      (if in-buffer
          (progn
            (if (eql direction 1)
                (setq chopper-lines-number (/ (- (line-number-at-pos (point-max))
                                                 (line-number-at-pos)) 2))
              (setq chopper-lines-number (/ (line-number-at-pos) 2)))
            (forward-line (* chopper-lines-number direction)))
        (move-to-window-line (round (setq chopper-lines-number
                                          (/ (1+ (save-excursion
                                                   (move-to-window-line -1)))
                                             2.0)))))
    ;; consecutive chops
    (forward-line (round (* (setq chopper-lines-number
                                  (/ chopper-lines-number 2))
                            direction)))))

(defun chopper-new-p (command)
  (or current-prefix-arg
      (not (eq last-command command))))

(provide 'chopper)

;;; chopper.el ends here
