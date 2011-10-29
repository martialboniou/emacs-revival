;;; mars-tiling.el --- 
;; 
;; Filename: mars-tiling.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 15:17:41 2011 (+0100)
;; Version: 
;; Last-Updated: Sat Feb 19 15:30:41 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 1
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: extends `tiling.el'
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

(require 'tiling)

(defconst mars-tiling-version 0.5)

;;;###autoload
(defgroup mars-tiling nil
  "Extends the tiling mode."
  :group 'convenience)

;;;###autoload
(defcustom mars-tiling-master-left 70
  "Ratio of the left window used in `tiling-master-left'. As percent unit, between 0 and 100."
  :type 'integer
  :group 'mars-tiling)

;;;###autoload
(defcustom mars-tiling-master-top 70
  "Ratio of the top window used in `tiling-master-top'. As percent unit, between 0 and 100."
  :type 'integer
  :group 'mars-tiling)

;;;###autoload
(defcustom mars-tiling-favorite-main-layouts (list 'tiling-master-left 'tiling-tile-4)
  "Main tiling layouts mainly used in `tiling-cycle'. An ordered list of functions."
  :type '(repeat (function :tag "Window manipulation function"))
  :group 'mars-tiling)

;;;###autoload
(defcustom mars-tiling-favorite-secondary-layouts (list 'tiling-master-top 'tiling-tile-4)
  "Secondary tiling layouts mainly used in `tiling-cycle'. An ordered list of functions."
  :type '(repeat (function :tag "Window manipulation function"))
  :group 'mars-tiling)

;;;###autoload
(defadvice tiling-cycle (around special-layouts-tiling (&optional numOfWins special-layouts) activate)
  (interactive "P")
  (let ((tiling-layouts (if (null special-layouts)
                           tiling-layouts
                          special-layouts)))
    ad-do-it))

;;;###autoload
(defadvice tiling-master (around mars-tiling-master (bufs horizontal) activate)
  "redefine 'tiling-master to expand left and top window to a willing size in percent
using `mars-tiling-master-left' and `mars-tiling-master-top' respectively. No more
minibuffer resizing (with new BALANCE-WINDOWS)."
  ;; (when horizontal
  ;;   (setq minibuf-height (window-height (minibuffer-window))))
  ad-do-it
  (flet ((crop-percent (num) (cond ((> num 100) 100)
                                   ((< num 0)   1)
                                   (t           num)))
         (find-new-size (ratio orig-size) (floor (* orig-size (/ ratio 100.0)))))
    (if horizontal
        (let (;; (h-factor (- minibuf-height
              ;;              (window-height (minibuffer-window))))
              (w-factor (- (find-new-size
                            (crop-percent mars-tiling-master-left)
                            (frame-width (selected-frame)))
                           (window-width))))
          ;; (shrink-window h-factor)
          (enlarge-window-horizontally w-factor))
      (let ((h-factor (- (find-new-size
                          (crop-percent mars-tiling-master-top)
                          (frame-height (selected-frame)))
                         (window-height))))
        (enlarge-window h-factor)))))

(provide 'mars-tiling)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mars-tiling.el ends here
