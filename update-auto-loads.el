;;; update-auto-loads.el --- 
;; 
;; Filename: update-auto-loads.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sun Feb 20 11:57:06 2011 (+0100)
;; Version: 
;; Last-Updated: Sat Mar  5 13:31:29 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 3
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: Generate `autoloads' to file local `loaddefs' in
;; 
;; Update autoloads by Trey Jackson at stackoverflow
;; CEDET case by martial.boniou at gmx.com
;; subdirectories-in-below-directory
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

(defun subdirectories-in-below-directory (directory &optional omit-if-file-named-list extrude-if-file-named-list)
  "Fetch all subdirectories from `directory'. This function is the subdirs version of 
`elips-files-in-below-directory' known as FILES-IN-BELOW-DIRECTORY seen in 
`http://www.rattlesnake.com/intro/Files-List.html'.
Omit directories with a file included in the list `omit-if-file-named-list'.
Extrude the first directory of a directory tree where a file named like in the list
`extrude-if-file-named-list'. In this case, the function returns a tuple containing
the non-omitted and non-extruded subdirectories on one side and an alist on the 
other side; those cons are like this: (FILENAME-AS-SYMBOL . LIST-OF-EXTRUDED-ROOT-DIR)."
  (interactive "DDirectory name: ")
  (let (subdirectories-list
	extrude-root-directory-alist
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
               ;; check if directory doesn't contain a file named else memoize
               ;; the directory in an alist, the filename being the key
               (let ((found nil))
                 (when extrude-if-file-named-list
                   (mapc '(lambda (x)
                            (when (and x (file-exists-p (concat dir-name "/" x)))
                              (let ((kv (assq (intern x) extrude-root-directory-alist)))
                                (if kv
                                    (setcdr (assq (intern x) extrude-root-directory-alist) (append (cdr kv) (list dir-name)))
                                  (setq extrude-root-directory-alist (append (list (cons (intern x) (list dir-name))) extrude-root-directory-alist))))
                              (setq found t)))
                         extrude-if-file-named-list))
                 found)
               ;; check if directory doesn't contain a file named like in the
               ;; `omit-if-file-named-list'
               (let ((found nil))
                 (when omit-if-file-named-list
                   (setq pending omit-if-file-named-list)
                   (while (and pending (null found))
                     (setq omit-file (pop pending))
                     (when (and omit-file (file-exists-p (concat dir-name "/" omit-file)))
                       (setq found t))))
                 found))
            (setq subdirectories-list
                  (cons (car (car current-directory-list))
                        (append
                         (subdirectories-in-below-directory
                          (car (car current-directory-list)))
                         subdirectories-list))))))
      (setq current-directory-list (cdr current-directory-list)))
    (if extrude-if-file-named-list
        (cons subdirectories-list extrude-root-directory-alist) ; (Path . ((Symbol . (Path))))
      subdirectories-list)))

;;;###autoload
(let ((new-path (expand-file-name (file-name-directory load-file-name))))
  (unless (member new-path (mapcar '(lambda (x) (file-name-as-directory x)) load-path))
    (setq load-path (cons new-path load-path))))

;;;###autoload
(defun update-autoloads-in-package-area (&optional file)
  "Update autoloads for files in the directory containing this file.
Add a file named `.cedet' at the root of directory where `cedet-update-autoloads'
must be used to generate the `loaddefs.el''s autoloads for itself and its
subdirectories instead of the standard `update-autoloads-from-directories'."
  (interactive)
  (save-excursion
    (let ((base (file-truename
		 (file-name-directory
		  (symbol-file 'update-autoloads-in-package-area 'defun)))))
      (unless (fboundp 'update-autoloads-from-directories)
	(require 'autoload))
      (let ((generated-autoload-file (concat base "loaddefs.el")))
        (unless (file-exists-p generated-autoload-file)
          (with-current-buffer (find-file-noselect generated-autoload-file)
            (insert ";;")                 ; create the file with non-zero size to appease autoload
            (save-buffer)))
        (cd base)
        (if file
            (update-file-autoloads file) 
          (let ((additional-directories (subdirectories-in-below-directory base '(".nosearch" ".noauto") '(".cedet")))
                ;; put a .cedet file at the root of your cedet directory
                (expanded-path (mapcar '(lambda (x) (file-name-as-directory (expand-file-name x)))
                                       load-path)))
            (update-autoloads-from-directories base)
            ;; normal case
            (dolist (dir (car additional-directories))
              (when (member (file-name-as-directory dir) expanded-path) ; belongs to `LOAD-PATH'
                (update-autoloads-from-directories dir)))
            ;; CEDET case
            (let ((cedet-bases (cdr (cadr additional-directories))))
              (dolist (dir cedet-bases)
                (message "CEDET autoloads for %s and its subdirs." dir)
                (unless (fboundp 'cedet-update-autoloads)
                  (load "cedet"))	; latest common/cedet in `LOAD-PATH'
                ;; FIXME: common/cedet rebuilds `LOAD-PATH'
                (cedet-update-autoloads generated-autoload-file dir)
                (let ((subdirs (subdirectories-in-below-directory dir '(".nosearch"))))
                  (message (prin1-to-string subdirs))
                  (dolist (subdir subdirs)
                    (when (member (file-name-as-directory subdir) expanded-path) ; belongs to `LOAD-PATH'
                      (cedet-update-autoloads generated-autoload-file subdir))))))))))))

;;;###autoload
(defun update-autoloads-for-file-in-package-area (file)
  (interactive "f")
  (update-autoloads-in-package-area file))

(provide 'update-auto-loads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; update-auto-loads.el ends here


