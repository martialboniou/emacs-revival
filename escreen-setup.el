(require 'escreen)
;; snippets source: http://tapoueh.org/blog/2009/09/22-escreen-integration.html

(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
        (emphased ""))

    (dolist (s escreens)
      (setq emphased
            (concat emphased (if (= escreen-current-screen-number s)
                                 (propertize (number-to-string s)
                                             'face 'info-title-3)
                               (number-to-string s))
                    " ")))
    (message "escreen: active screens: %s" emphased)))

(defadvice escreen-kill-screen (around ask-and-inform-screen-numbers nil activate)
  (interactive)
  (when (y-or-n-p "Do you really want to kill this screen? ")
    (progn
      ad-do-it)))

(add-hook 'escreen-goto-screen-hook
          #'escreen-get-active-screen-numbers-with-emphasis)

(provide 'escreen-setup)
