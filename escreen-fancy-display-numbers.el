(require 'escreen)
;; snippets source: http://tapoueh.org/blog/2009/09/22-escreen-integration.html

;;;###autoload
(defun esceen-fancy-display-numbers ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
        (ad-msg "escreen:"))
    (if (null (cdr escreens))
        (message "%s no other screens" ad-msg)
      (let ((emphased ""))
        (dolist (s escreens)
          (setq emphased
                (concat emphased (if (= escreen-current-screen-number s)
                                     (propertize (number-to-string s)
                                                 'face 'info-title-3)
                                   (number-to-string s))
                        " ")))
        (message "%s active screens: %s" ad-msg emphased)))))

(defadvice escreen-kill-screen (around ask-and-inform-screen-numbers nil activate)
  (interactive)
  (if (escreen-configuration-one-screen-p)
      ad-do-it
    (when (y-or-n-p "Do you really want to kill this screen? ")
      (progn
        ad-do-it))))

(add-hook 'escreen-goto-screen-hook
          #'esceen-fancy-display-numbers)

(provide 'escreen-setup)
