(require 'autopair)
(require 'viper)

;;; no AUTOPAIR action in VI-STATE
;;
(defun autopair-handle-viper (action pair pos-before)
  (unless (eq viper-current-state 'vi-state)
    (autopair-default-handle-action action pair pos-before)))

;;; AUTOPAIR-BACKSPACE MUST precede 'VIPER-DEL-BACKWARD-CHAR-IN-INSERT
;;  Idea: stephan-baumeister.com
;;  Motivation: `autopair-mode' makes 'emulation-mode-map-alists to append 'autopair-emulation-alist
(defadvice viper-change-state-to-insert (after autopair nil activate)
  (add-to-ordered-list 'emulation-mode-map-alists 'autopair-emulation-alist 300))

(eval-after-load "vimpulse"
  '(progn
     ;; viper keymaps in vimpulse-viper-function-redefinitions are pushed in front of the alist
     (defadvice vimpulse-normalize-minor-mode-map-alist (after order-viper--key-maps nil activate)
       (add-to-ordered-list 'emulation-mode-map-alists 'viper--key-maps 500)))) ; TODO: do the same in a CUA-viper-compat script

(add-to-list 'autopair-handle-action-fns #'autopair-handle-viper)

(provide 'autopair-viper-compat)
