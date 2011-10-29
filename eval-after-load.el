;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-04-05 19:40:28 Monday by ahei>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defvar loadfile-mode-map-hooks
  `(("cc-mode"         nil                    c-mode-base-map            c-mode-common-hook)
    ("cc-mode"         c-mode                 c-mode-map                 c-mode-hook)
    ("cc-mode"         c++-mode               c++-mode-map               c++-mode-hook)
    ("cc-mode"         java-mode              java-mode-map              java-mode-hook)
    "lisp-mode"                                                           
    ("lisp-mode"       emacs-lisp-mode        emacs-lisp-mode-map        emacs-lisp-mode-hook)
    "help-mode"                                                           
    ("man"             Man-mode               Man-mode-map               Man-mode-hook)
    "log-view"                                                            
    ("compile"         compilation-mode       compilation-mode-map       compilation-mode-hook)
    ("gud"             nil                    nil                        gdb-mode-hook)
    ("lisp-mode"       lisp-interaction-mode  lisp-interaction-mode-map  lisp-interaction-mode-hook)
    "browse-kill-ring"
    ("simple"          completion-list-mode   completion-list-mode-map   completion-list-mode-hook)
    ("hideshow"        nil                    nil                        hs-hide-hook)
    ("inf-ruby"        inferior-ruby-mode     inferior-ruby-mode-map     inferior-ruby-mode-hook)
    ("cus-edit"        custom-mode            custom-mode-map            custom-mode-hook)
    ("info"            Info-mode              Info-mode-map              Info-mode-hook)
    ("psvn"            svn-log-edit-mode      svn-log-edit-mode-map      svn-log-edit-mode-hook)
    ("package"         package-menu-mode      package-menu-mode-map      package-menu-mode-hook)
    "dired"             
    ("apropos"         nil                    nil                        apropos-mode-hook)
    ("psvn"            svn-log-view-mode      svn-log-view-mode-map      svn-log-view-mode-hook)
    "diff-mode"         
    ("ibuffer"         nil                    nil                        ibuffer-mode-hook)
    ("sgml-mode"       html-mode              html-mode-map              html-mode-hook)
    "sgml-mode"
    "w3m"
    ("data-debug"      data-debug-mode        nil                        data-debug-mode-hook)
    ("debug"           debugger-mode          debugger-mode-map          debugger-mode-hook)
    "text-mode"
    "color-theme"
    "woman"
    "doxymacs")
  "*List used to find load file by mode or map or hook.

Every element of list is or consist by load file, mode, map and hook,
or just one load file, or nil.")

;;;###autoload
(defun eval-after-load-by-modes (modes fun)
  "Run `eval-after-load' on function FUN by MODES."
  (if (listp modes)
      (eval-after-load-by-symbols modes 1 fun)
    (eval-after-load-by-symbol modes 1 fun)))

;;;###autoload
(defun eval-after-load-by-maps (maps fun)
  "Run `eval-after-load' on function FUN by MAPS."
  (if (listp maps)
      (eval-after-load-by-symbols maps 2 fun)
    (eval-after-load-by-symbol maps 2 fun)))

;;;###autoload
(defun eval-after-load-by-hooks (hooks fun)
  "Run `eval-after-load' on function FUN by HOOKS."
  (if (listp hooks)
      (eval-after-load-by-symbols hooks 3 fun)
    (eval-after-load-by-symbol hooks 3 fun)))

;;;###autoload
(defun eval-after-load-by-symbols (symbols pos fun)
  "Run `eval-after-load' on function FUN by SYMBOLS."
  (mapc
   `(lambda (symbol)
      (eval-after-load-by-symbol symbol ,pos ,fun))
   symbols))

;;;###autoload
(defun eval-after-load-by-symbol (symbol pos fun)
  "Run `eval-after-load' on function FUN by SYMBOL."
  (let (file)
    (setq file (find-loadfile-by-symbol symbol pos))
    (if file
        (eval-after-load file `(funcall ,fun ',symbol))
      (funcall fun symbol))))

;;;###autoload
(defun find-loadfile-by-mode (mode)
  "Find load file by mode MODE."
  (find-loadfile-by-symbol mode 1))

;;;###autoload
(defun find-loadfile-by-map (map)
  "Find load file by map MAP."
  (find-loadfile-by-symbol map 2))

;;;###autoload
(defun find-loadfile-by-hook (hook)
  "Find load file by hook HOOK."
  (find-loadfile-by-symbol hook 3))

;;;###autoload
(defun find-loadfile-by-symbol (symbol pos)
  "Find load file by symbol SYMBOL, its position is POS."
  (catch 'get
    (let ((symbol-name (symbol-name symbol)))
      (dolist (pair loadfile-mode-map-hooks)
        (if (stringp pair)
            (if (string= symbol-name (get-name-by-loadfile pair pos))
                (throw 'get pair)
              (let ((file (progn (and (string-match "^\\(.+\\)-mode$" pair)
                                      (match-string 1 pair)))))
                (if file
                    (if (string= symbol-name (get-name-by-loadfile file pos))
                        (throw 'get pair)))))
          (if pair
              (if (eq (nth pos pair) symbol)
                  (throw 'get (car pair)))))))))

(defun get-name-by-loadfile (file pos)
  "Get `symbol-name' by load file FILE and position POS."
  (let ((part
         (case pos
           (1 "mode")
           (2 "mode-map")
           (3 "mode-hook"))))
    (concat file "-" part)))

;;;###autoload
(defun add-hooks-after-load (hooks fun)
  "Add FUN to HOOKS by `eval-after-load-by-hooks'."
  (eval-after-load-by-hooks
   hooks
   `(lambda (hook)
      (add-hook hook ',fun))))

(defun define-keys-after-load (maps key-pairs)
  "Add FUN to HOOK by `eval-after-load-by-hook'."
  (eval-after-load-by-maps
   maps
   `(lambda (map)
      (define-key-list (symbol-value map) ',key-pairs))))

(provide 'eval-after-load)
