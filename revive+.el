;;; revive+.el ---
;;
;; Filename: revive+.el
;; Description: Selected window memoization for revive
;; Author: Martial Boniou
;; Maintainer: Martial Boniou
;; Created: Thu Mar 10 12:12:09 2011 (+0100)
;; Version: 0.9
;; Last-Updated: Thu Mar 10 15:09:20 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 26
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: Let revive preserve the window focus to restore
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'revive)

(defun current-window-configuration-printable ()
  (let ((curwin (selected-window))
        (wlist (revive:window-list)) (edges (revive:all-window-edges)) buflist)
    (save-excursion
      (while wlist
        (select-window (car wlist))
                                        ;should set buffer on Emacs 19
        (set-buffer (window-buffer (car wlist)))
        (let ((buf (list
                    (if (and
                         (buffer-file-name)
                         (fboundp 'abbreviate-file-name))
                        (abbreviate-file-name
                         (buffer-file-name))
                      (buffer-file-name))
                    (buffer-name)
                    (point)
                    (window-start))))
          (when (eq curwin (selected-window))
            (setq buf (append buf (list nil 'focus))))
          (setq buflist
                (append buflist (list buf))))
        (setq wlist (cdr wlist)))
      (select-window curwin)
      (list (revive:screen-width) (revive:screen-height) edges buflist))))

(defun restore-window-configuration (config)
  (let ((width (car config)) (height (nth 1 config))
        (edges (nth 2 config)) (buflist (nth 3 config)) buf)
    (set-buffer (get-buffer-create "*scratch*"))
    (setq edges (revive:normalize-edges width height edges))
    (construct-window-configuration edges)
    (revive:select-window-by-edge (revive:minx) (revive:miny))
    (let (focus)
      (while buflist
        (setq buf (pop buflist))
        (cond
         ((and (revive:get-buffer buf)
               (get-buffer (revive:get-buffer buf)))
          (switch-to-buffer (revive:get-buffer buf))
          (when (eq 'focus (car (last buf)))
            (setq focus (selected-window)))
          (goto-char (revive:get-window-start buf)) ; to prevent high-bit missing
          (set-window-start nil (point))
          (goto-char (revive:get-point buf)))
         ((and (stringp (revive:get-file buf))
               (not (file-directory-p (revive:get-file buf)))
               (revive:find-file (revive:get-file buf)))
          (set-window-start nil (revive:get-window-start buf))
          (goto-char (revive:get-point buf))))
        (other-window 1))
      (when focus
        (select-window focus)))))

(provide 'revive+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revive+.el ends here
