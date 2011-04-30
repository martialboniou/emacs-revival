;;; sticky-control.el --- apply C- modifier without using the [ctrl] key

;; Copyright (C) 2010 Ryan Johnson

;; Author: Ryan Johnson
;; Version: 1.0
;; Keywords: keyboard control

;; License: Creative Commons Attribution 3.0

;; Basically, you can use or modify this code freely, but must
;; acknowledge where you got it from.

;; See http://creativecommons.org/licenses/by/3.0/

;;; Comentary:

;; Emacs encourages (strongly!) pressing multiple keys simultaneously,
;; which is actually a Really Bad Idea (tm) for two reasons.

;; First, it's a terrible ergonomic and leads to repetitive stress
;; injuries (= carpal tunnel). Numb or tingling elbows and wrists are
;; the classic symptom.

;; Second, it's not very accessible for folks who have trouble typing
;; fast, with two hands, etc.

;; In theory, it's possible to always hit the [ctrl] key with one hand
;; and the key of interest with the other, but I found it easier to
;; remove the offending [ctrl] key altogether than to discipline
;; myself that way. Plus, it doesn't fix the accessibility problem,
;; and some operating systems (*cough* linux *cough*) have an annoying
;; tendency to treat right-ctrl and left-ctrl differently.

;; What we really need is a key (sequence) which is to [ctrl] like ESC
;; is to [meta] -- "sticky." The built-in sequence C-x @ c technically
;; does this, but is useless because it still requires typing C-x.

;; Instead, we observe that some keys -- like ?j -- almost never get
;; pressed twice in succession and yet sit right under a strong
;; finger. So, we wire up one such key so that, when pressed twice
;; quickly, it applies the control modifier to whatever comes next. 
;;
;; For example, sticky-control-mode would start an interactive search
;; in response to the key sequence "jjs" (or even "js" if you
;; configure it as a shortcut). Saving a file would become "jxjs",
;; etc. Unrecognized combinations, such as "jump" are passed through,
;; and recognized combinations also pass through if typed slowly
;; enough. 

(defun sticky-control-setup (installing)
  "Set up the `key-translation-map' for `sticky-control-mode'"
  (define-key
    key-translation-map
    (vector sticky-control-key)
    (if installing 'sticky-control-do-it nil)))

(defun sticky-control-do-it (prompt)
  "The brains behind `sticky-control-mode'"
  (interactive "p")
  (let* ((e1 last-input-event)
	 (e2 (read-event nil nil sticky-control-timeout))
	 (v2 (if (vectorp e2) e2 (vector e2)))
	 tmp)
    (cond
     ((not e2)
      (this-command-keys))
     ((= e1 e2)
      (event-apply-control-modifier nil))
     ((setq tmp (lookup-key (cons 'keymap sticky-control-shortcuts) v2))
      tmp)
     (t
      (setq unread-command-events (cons e2 unread-command-events))
      (this-command-keys))
     )))

(defvar sticky-control-keymap nil
  "The keymap which powers `sticky-control-mode'")

(defun sticky-control-set-key (sym val)
  "Set the `sticky-control-key' used by `sticky-control-mode'"
  (unless (eq sym 'sticky-control-key)
    (error "Unknown symbol: %S" sym))
  (unless (characterp val)
    (error "sticky-control-key not a character: %S" val))
  
  ;; remove the old binding
  (let ((active (and (boundp 'sticky-control-mode)
		     sticky-control-mode)))
    (if active (sticky-control-setup nil))
    ;; set the new value
    (setq sticky-control-key val)
    ;; install the new binding
    (if active (sticky-control-setup t))
  ))

(defgroup sticky-control nil
  "Apply C- modifier without using the [ctrl] key"
  :group 'convenience)

(defcustom sticky-control-shortcuts
  '((?c . "\C-c")
    (?g . "\C-g")
    (?x . "\C-x")
    (?r . "\C-r")
    (?s . "\C-s"))
  "Shortcuts available under `sticky-control-mode' which can reduce
the number of keystrokes needed to get things done.

For example, using ?j as the `sticky-control-key', the user would
normally have to type \"jjs\" to emulate \"C-s\". The shortcut
'(?s.\"C-s\") shortens that to \"js\" -- at the cost of
increasing the number of key combinations which can inadvertently
trigger sticky-control mode.

Entries should take the form of valid keymap bindings, or
'(key-sequence . binding), with the binding another key sequence,
a command, another keymap, etc."
  :type '(alist :tag "Mapping"
		:key-type (choice character
				  key-sequence)
		:value-type (choice character
				    key-sequence
				    function)
		)
  :group 'sticky-control
  :require 'sticky-control
  )

(defcustom sticky-control-key ?j
  "The hotkey to use when `sticky-control-mode' is active.

It should be a character which seldom occurs twice in a row and
is easily typed in quick succession. Good examples include ?j or
?v, while characters such as ?s and ?t make exceptionally bad
choices."
  :type 'character
  :options '(?j)
  :set 'sticky-control-set-key
  :require 'sticky-control
  :group 'sticky-control)

(defcustom sticky-control-timeout 0.5
  "Successive presses of `sticky-control-key' which occur further
apart than this are treated as independent keystrokes."
  :type 'number
  :options '(0.5)
  :require 'sticky-control
  :group 'sticky-control)

(define-minor-mode sticky-control-mode
  "Toggle sticky control mode.

When sticky control mode is active, pressing a certain hotkey
twice in quick succession (see `sticky-control-key') applies the
control modifier to the succeeding input event. This allows the
typist to avoid pressing multiple keys simultaneously, a bad
habit which destroys the wrists and which emacs normally
encourages.

For example, the key sequence \"jjs\" is translated to
\"C-s\" (normally `isearch-forward'), as is \"js\" under the
default shortcut settings (see `sticky-control-shortcuts').
Similarly, \"C-x C-s\" (normally `save-buffer') can be emulated
by typing \"jjxjjs\" or \"jxjs\". Unrecognized sequences, such as
\"jump\" are passed through, and recognized sequences also pass
through if typed slowly enough (see `sticky-control-timeout')."
  :global t
  :group 'sticky-control
  :require 'sticky-control
  (sticky-control-setup sticky-control-mode)
  (message "sticky-control-mode %s" (if sticky-control-mode "ON" "OFF"))
  )

(provide 'sticky-control)
