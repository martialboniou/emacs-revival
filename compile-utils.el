;;; compile-utils.el --- 
;; 
;; Filename: compile-utils.el
;; Description: Used by emacs-compile-directory to build `load-path'
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Mar  5 12:10:04 2011 (+0100)
;; Version: 
;; Last-Updated: Fri Nov  4 14:07:36 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 13
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: Put this file at the root of the site-lisp whose the
;;              subdirectories need to be set in the `load-path'.
;;              Use .nosearch to avoid a directory and its subdirs.
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
(setq load-path
      (append 
       (flet ((inspect-directory
               (directory) 
               (let ((omit-clause-list '(".nosearch"))
                     subdirectories-list
                     (current-directory-list
                      (directory-files-and-attributes directory t)))
                 (while current-directory-list
                   (when
                       (eq t (car (cdr (car current-directory-list))))
                     (let ((dir-name (car (car current-directory-list))))
                       (unless
                           (or
                            ;; check if directory is not a well-known archive one
                            (equal "." (substring dir-name -1))
                            (member (substring dir-name -4) '("/RCS" "/CVS" "/rcs" "/cvs"))
                            (member (substring dir-name -5) '("/.git" "/.svn"))
                            (equal (substring dir-name -6) "/_darcs")
                            (let ((found nil))
                              (setq pending omit-clause-list)
                              (while (and pending (null found))
                                (setq omit-file (pop pending))
                                (when (and omit-file (file-exists-p (concat dir-name "/" omit-file)))
                                  (setq found t)))
                              found))
                         (setq subdirectories-list
                               (cons (car (car current-directory-list))
                                     (append
                                      (inspect-directory
                                       (car (car current-directory-list)))
                                      subdirectories-list))))))
                   (setq current-directory-list (cdr current-directory-list)))
                 subdirectories-list))) (inspect-directory
                                         (file-name-directory load-file-name)))
       load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile-utils.el ends here
