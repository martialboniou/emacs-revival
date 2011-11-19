;;; revive+.el ---
;;
;; Filename: revive+.el
;; Description: Selected window memoization for revive
;; Author: Martial Boniou
;; Maintainer: Martial Boniou
;; Created: Thu Mar 10 12:12:09 2011 (+0100)
;; Version: 0.9
;; Last-Updated: Fri Nov 18 20:34:08 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 58
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

(defconst revive-plus:version
  "revive+.el,v 0.9 <hondana@gmx.com>"
  "Version of revive+.el")

(defgroup revive-plus nil
  " Revive window configurations anywhere."
  :group 'convenience)

(defcustom revive-plus:all-frames nil
  "Revive all frames and their window-configurations.
If NIL, saving and restoring will be enabled for the currently
focused frame."
  :group 'revive-plus
  :type 'boolean)

(defun revive:window-list (&optional frame)
  "Return the all window list in sorted order."
  (let*((curwin (if (null frame)
                    (selected-window)
                  (frame-selected-window frame)))
        (win curwin) wlist)
    (if (null
     (catch 'found
       (while t
         (if (and (= (revive:minx) (car (revive:window-edges win)))
              (= (revive:miny) (car (cdr (revive:window-edges win)))))
         (throw 'found t))
         (if (eq (setq win (next-window win)) curwin)
         (throw 'found nil)))))
    (error "Unexpected window configuration."))
    (setq curwin win wlist (list win))
    (while (not (eq curwin (setq win (next-window win))))
      (setq wlist (append wlist (list win)))) ;use append to preserve order
    wlist))

(defun revive:window-buffer-list (&optional frame)
  "Return the all shown buffer list.
Each element consists of '(buffer-file-name window-start point)"
  (let ((curw (if (null frame)
                  (selected-window)
                (frame-selected-window frame)))
        (wlist (revive:window-list)) wblist)
    (save-excursion
      (while wlist
    (select-window (car wlist))
    (set-buffer (window-buffer (car wlist))) ;for Emacs 19
    (setq wblist
          (append wblist
              (list (list
                 (if (and (fboundp 'abbreviate-file-name)
                      (buffer-file-name))
                 (abbreviate-file-name (buffer-file-name))
                   (buffer-file-name))
                 (window-start)
                 (point))))
          wlist (cdr wlist)))
      (select-window curw)
      wblist)))

(defun revive:select-window-by-edge (x y &optional frame)
  "Select window whose north west corner is (X, Y).
If the matching window is not found, select the nearest window."
  (let*((curwin (if (null frame)
                    (selected-window)
                  (frame-selected-window frame)))
        (win (next-window curwin)) edges
    s2 (min 99999) minwin)
    (or
     (catch 'found
       (while t
     (setq edges (revive:window-edges win)
           s2 (+ (* (- (car edges) x) (- (car edges) x))
             (* (- (nth 1 edges) y) (- (nth 1 edges) y))))
     (cond
      ((= s2 0)
       (select-window win)
       (throw 'found t))
      ((< s2 min)
       (setq min s2 minwin win)))
     (if (eq win curwin) (throw 'found nil)) ;select the nearest window
     (setq win (next-window win))))
     (select-window minwin))))

(defun revive:all-window-edges (&optional frame)
  "Return the all windows edges by list."
  (let ((wlist (revive:window-list frame)) edges)
    (while wlist
      (setq edges (append edges (list (revive:window-edges (car wlist))))
        wlist (cdr wlist)))
    edges))

(defun current-window-configuration-printable (&optional single-frame)
  "Print window configuration for a frame. Override the same
function defined in `revive'."
  (let ((curwin (if (or (null single-frame) (not (framep single-frame)))
                    (selected-window)
                  (frame-selected-window single-frame)))
        (wlist (revive:window-list single-frame))
        (edges (revive:all-window-edges single-frame)) buflist)
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

(defun window-configuration-printable ()
  "Print window configuration for the current frame.
If REVIVE-PLUS:ALL-FRAMES is true, print window configuration for
all frames as a list of current-window-configuration-printable."
  (if (null revive-plus:all-frames)
      (current-window-configuration-printable)
    (let ((focus (selected-frame)))
      (mapcar #'(lambda (frame)
                  (current-window-configuration-printable frame))
               (cons focus (remq focus (frame-list)))))))

(defun restore-window-configuration (config)
  (if (listp (car config))
      ;; TODO: enable persistency for NO-WINDOW-SYSTEM case
      (progn
        (unless (null (cdr (frame-list)))
          (delete-other-frames))
        (restore-window-configuration (car config))
        (unless (null (cdr config))
          (mapc #'(lambda (cframe)
                    (make-frame)
                    (restore-window-configuration cframe))
                (cdr config))
          (next-frame)))
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
          (select-window focus))))))

(provide 'revive+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revive+.el ends here
