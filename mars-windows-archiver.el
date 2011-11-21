;;; mars-windows-archiver.el --- 
;; 
;; Filename: mars-windows-archiver.el
;; Description: OBSOLETE
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 14:27:14 2011 (+0100)
;; Version: 
;; Last-Updated: Mon Nov 21 17:33:29 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 14
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: Save/restore window configurations
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

(require 'revive+)

(defgroup mars-windows-archiver nil
  "Archive window configurations."
  :group 'convenience)

(defun configuration-file-name (name &optional dir)
  "Generates a complete name for a configuration file according to the `Emacs' version."
  (let ((root-filename (concat "~/.emacs-" name))
        (subdir        (concat "~/.emacs.d/"
                               (if dir (file-name-as-directory dir) "")))
        (x-subdir      (concat "~/.xemacs/"
                               (if dir (file-name-as-directory dir) ""))))
    (cond
     ((file-exists-p root-filename) root-filename)
     ((and (not (featurep 'xemacs))
           (file-directory-p subdir))
      (concat subdir name))
     ((and (not (featurep 'xemacs))
           (boundp 'user-emacs-directory))
      (concat user-emacs-directory name))
     ((and (featurep 'xemacs) (file-directory-p x-subdir))
      (concat x-subdir name))
     (t root-filename))))

(defcustom mars-windows-archiver-file
  (configuration-file-name "windows-archiver" "data")
  "*File name where window configurations are saved to and loaded from.

If you want your window configurations shared between Emacs and XEmacs,
customize this value and make sure that `mars-windows-archiver-coding-system'
is set to a coding system that exists in both emacsen."
  :type 'file
  :group 'mars-windows-archiver)

(defcustom mars-windows-register-limit 10
  "number of slots in the `mars-windows-register'."
  :type 'integer
  :group 'mars-windows-archiver)

(defvar mars-windows-archiver-coding-system
  (if (and (featurep 'xemacs)
           (<= emacs-major-version 21)
           (< emacs-minor-version 5))
      'iso-2022-8                       ; XEmacs
    (if (coding-system-p 'utf-8)
        'utf-8                          ; Emacs 22
      'emacs-mule)))                    ; Emacs

(defvar mars-windows-register nil
  "A list for `mars-windows-archiver'.")

(defcustom kiwon/last-window-configuration-file
  (configuration-file-name "last-window-configuration" "data")
  "Script to restore the window configuration at startup."
  :type 'file
  :group 'mars-windows-archiver)

(defun mars-windows-archiver-save (&optional dont-alert)
  (interactive)
  (when (or dont-alert (y-or-n-p "Archive the current window configuration? "))
    (setq mars-windows-register (cons (current-window-configuration-printable) mars-windows-register))
    (let ((out-num (- (length mars-windows-register) mars-windows-register-limit)))
      (nbutlast mars-windows-register out-num))
    (unless (and (null mars-windows-register) (not (file-writable-p mars-windows-archiver-file)))
      (with-temp-buffer
        (set-buffer-file-coding-system mars-windows-archiver-coding-system)
        (insert (prin1-to-string mars-windows-register))
        (write-region (point-min) (point-max) mars-windows-archiver-file)))))

(defun mars-windows-archiver-clear ()
  (interactive)
  (when (yes-or-no-p "Delete the archived window configurations? ")
    (when (yes-or-no-p "Are you really sure? ")
      (setq mars-windows-register nil)
      (condition-case nil
          (delete-file mars-windows-archiver-file)
        (error
         (message "Information: [mars] mars-windows-archiver-clear: no window configurations archive file to delete"))))))

(defun mars-windows-archiver-restore (&optional num)
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (read-number "Which window configuration number (0 is the last saved one): "))))
  (unless (integerp num)
    (setq num 0))
  (let ((wconf (nth num mars-windows-register)))
    (unless (null wconf)
      (restore-window-configuration wconf))))

(defun mars-windows-archiver-load ()
  (interactive)
  (when (yes-or-no-p "Reload previous archived window configurations? ")
    (mars-windows-archiver-load-in-session)))

(defun mars-windows-archiver-load-in-session ()
  (when (and (file-exists-p mars-windows-archiver-file)
             (file-readable-p mars-windows-archiver-file))
    (with-temp-buffer
      (set-buffer-file-coding-system mars-windows-archiver-coding-system)
      (insert-file-contents mars-windows-archiver-file)
      (setq mars-windows-register (read (current-buffer))))))

;; Goal: load/save configuration at desktop startup/quitting
;; - By Kiwon Um
(defun kiwon/save-window-configuration ()
  (write-region (concat "(restore-window-configuration '"
                        (prin1-to-string (current-window-configuration-printable))
                        ")")
                nil kiwon/last-window-configuration-file))

(defun kiwon/restore-window-configuration ()
  (let ((fi kiwon/last-window-configuration-file))
    (when (file-exists-p fi)
      (load-file fi))))

(provide 'mars-windows-archiver)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mars-windows-archiver.el ends here
