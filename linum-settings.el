;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-04-05 19:13:46 Monday by ahei>

(if (>= emacs-major-version 23)
    (require 'linum)
  (require 'linum "linum-for-22"))

(global-set-key (kbd "C-x N") 'linum-mode)

(defun linum-settings ()
  "settings for `linum'."

  ;; http://emacser.com/linum-plus.htm
  (require 'linum+)

  (add-hooks-after-load                 ; need `eval-after-load' to be parsed for autoloads
   `(find-file-hook help-mode-hook Man-mode-hook log-view-mode-hook
                    compilation-mode-hook gdb-mode-hook lisp-interaction-mode-hook
                    browse-kill-ring-mode-hook completion-list-mode-hook hs-hide-hook
                    inferior-ruby-mode-hook custom-mode-hook Info-mode-hook svn-log-edit-mode-hook
                    package-menu-mode-hook dired-mode-hook apropos-mode-hook svn-log-view-mode-hook
                    diff-mode-hook emacs-lisp-mode-hook ibuffer-mode-hook html-mode-hook
                    w3m-mode-hook data-debug-hook debugger-mode-hook text-mode-hook color-theme-mode-hook)
   (lambda()
     (unless (eq major-mode 'image-mode)
       (linum-mode 1)))))

(eval-after-load 'linum
  '(linum-settings))

(provide 'linum-settings)
