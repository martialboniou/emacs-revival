;;; alias-minor-modes.el --- 
;; 
;; Filename: alias-minor-modes.el
;; Description: alias minor mode names in the mode line
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Thu Oct 20 15:41:46 2011 (+0200)
;; Version: 0.1
;; Last-Updated: Thu Oct 20 20:28:16 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 31
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
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

(eval-when-compile (require 'cl))

;;;###autoload
(defmacro alias-minor-mode (mode minor-symbol)
  "Replaces minor mode MODE by a new name based on MINOR-SYMBOL to be displayed in the mode line. Eg. (alias-minor-mode undo-tree UT)"
  (let* ((m-name (symbol-name mode))
         (m-mode (intern (concat m-name "-mode"))))
    `(eval-after-load ,m-name
       '(when (assoc ',m-mode minor-mode-alist)
          (setcdr (assoc ',m-mode minor-mode-alist) '(,(concat " " (symbol-name minor-symbol))))))))

;;;###autoload
(defun alias-minor-modes (pairs)
  "Map a list of PAIRS on the `alias-minor-mode' function.
Eg. (alias-minor-modes '(undo-tree UT paredit PE))"
  (if (null (cdr pairs))
      t
    (let ((minor-mode (car pairs))
          (minor-name (cadr pairs)))
      (eval `(alias-minor-mode ,minor-mode ,minor-name))
      (alias-minor-modes (cddr pairs)))))

(provide 'alias-minor-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; alias-minor-modes.el ends here
