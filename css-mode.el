;;; css-mode.el --- Major mode for editing Cascading Style Sheets

;; Copyright (C) 2005  
;;  Karl Landstrom.

;; Author:  Karl Landström <kland at comhem dot se>
;; Maintainer: Karl Landström <kland at comhem dot se>
;; Version: 1.0
;; Keywords: languages, hypermedia
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Installation:

;; Put this file in a folder where Emacs can find it.  On GNU/Linux
;; it's usually /usr/local/share/emacs/site-lisp/ and on Windows it's
;; something like "C:\Program Files\Emacs<version>\site-lisp".  To
;; make it run slightly faster you can also compile it from Emacs (M-x
;; `emacs-lisp-byte-compile'). Then add
;; 
;;    (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
;;    (autoload 'css-mode "css-mode" nil t)

;; to your .emacs initialization file (_emacs on Windows).

;;; Code:

;;Other js-mode addition from xemacs js mode for speedbar
(require 'comint)

(eval-when-compile
  (require 'regexp-opt)
  (require 'font-lock)
  (require 'speedbar)
  )
;;finish

;; Speedbar handling
(if (fboundp 'speedbar-add-supported-extension)
    (speedbar-add-supported-extension '(".css")))


(defcustom css-indent-level 3 "Number of spaces for each indent step.")


;; KEYMAP

(defvar css-mode-map nil "Keymap in CSS mode.")

(unless css-mode-map (setq css-mode-map (make-sparse-keymap)))


;; SYNTAX TABLE AND PARSING

(defvar css-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?- "w" table)
    table)
  "Syntax table used in CSS mode.")


;; The css-re-search-* functions are copied from my JavaScript mode,
;; `javascript.el'.

(defun css-re-search-forward-inner (regexp &optional bound count)
  "Used by `css-re-search-forward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-forward 
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse))) 
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            (t
             (setq count (1- count))))
      (setq saved-point (point))))
  (point))


(defun css-re-search-forward (regexp &optional bound noerror count)
  "Invokes `re-search-forward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr 
         (cond ((null count)
                '(css-re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(css-re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(css-re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun css-re-search-backward-inner (regexp &optional bound count)
  "Used by `css-re-search-backward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-backward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse))) 
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse) 
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            (t
             (setq count (1- count))))))
  (point))


(defun css-re-search-backward (regexp &optional bound noerror count)
  "Invokes `re-search-backward' but treats the buffer as if strings
and comments have been removed."
  (let ((saved-point (point))
        (search-expr 
         (cond ((null count)
                '(css-re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(css-re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(css-re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


;; FONT LOCK

(defconst css-font-lock-keywords-1
  (list

   ;; selectors
   (list "^[ \t]*[[:word:].#:]+[ \t]*\\([,{]\\|/\\*\\|$\\)"
         (list "\\([[:word:].#:]+\\)[ \t]*\\(,\\|{.*\\|/\\*.*?\\*/\\|$\\)"
               '(progn
                  (backward-char)
                  (if (nth 1 (parse-partial-sexp (point-min) (point)))
                      (end-of-line)
                    (beginning-of-line)))
               '(end-of-line)
               '(1 font-lock-function-name-face)))

   ;; properties
   (list "\\(\\w+\\)[ \t]*:" 1 font-lock-variable-name-face)

   ;; values that need to be quoted
   (list ":[ \t]*\\(\\w+[ \t]+\\w+\\)" 1 font-lock-warning-face)

   ;; values (unquoted)
;;    (list ":[ \t]*\\(\"\\|\\w" 
;;          "\\(.+?\\)\\(,\\|;\\|}\\|$\\)"
;;          '(backward-char)
;;          '(end-of-line)
;;          '(1 font-lock-string-face))
))

(defconst css-font-lock-keywords-2 css-font-lock-keywords-1)
(defconst css-font-lock-keywords-3 css-font-lock-keywords-1)
(defconst css-font-lock-keywords 
  '(css-font-lock-keywords-3 css-font-lock-keywords-1 css-font-lock-keywords-2
                             css-font-lock-keywords-3)
  "See `font-lock-keywords'.")


(defun css-indent-line ()
  (interactive)
  (let ((indent
         (save-excursion
           (back-to-indentation)
           (let ((p (parse-partial-sexp (point-min) (point)))
                 (end-brace-p (looking-at "}")))
             (cond
              ((or (nth 8 p) (looking-at "/[/*]"))
               (current-indentation))
              ((save-excursion
                 (and (skip-chars-backward " \t\n:,")
                      (looking-at "[:,]")))
               (save-excursion 
                 (css-re-search-backward "^[ \t]*\\w")
                 (+ (current-indentation) css-indent-level)))
              ((nth 1 p)
               (save-excursion 
                 (goto-char (nth 1 p))
                 (+ (current-indentation) (if end-brace-p 0 css-indent-level))))
              (t
               0)))))
        (offset (- (current-column) (current-indentation))))
    (indent-line-to indent)
    (if (> offset 0) (forward-char offset))))


(defconst css-imenu-generic-expression
  '((nil "^[ \t]*\\([[:word:].:#, \t]+\\)\\s-*{" 1))
  "Regular expression matching any selector. Used by imenu.")

;;;###autoload
(defun css-mode ()
  "Major mode for editing CSS source code.

Key bindings:

\\{css-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (use-local-map css-mode-map)
  (set-syntax-table css-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'css-indent-line)
  (set (make-local-variable 'font-lock-defaults) (list css-font-lock-keywords))

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-generic-expression)
       css-imenu-generic-expression)

  (when (featurep 'newcomment)
    (set (make-local-variable 'comment-start) "/*")
    (set (make-local-variable 'comment-end) "*/")
    (set (make-local-variable 'comment-start-skip) "/\\*+ *")
    (set (make-local-variable 'comment-end-skip) " *\\*+/")
    (require 'advice)

    (defadvice comment-dwim (after inhibit-indent)
      "Indent new comment lines to column zero and insert only one space
before a trailing comment."
      (when (eq major-mode 'css-mode)
        (let ((prs (parse-partial-sexp 
                    (save-excursion (beginning-of-line) (point)) 
                    (point))))
          (when (nth 4 prs)
            (save-excursion 
              (goto-char (nth 8 prs))
              (when (looking-at "/\\*[ \t]*\\*/")
                (if (= (current-column) (current-indentation))
                    (indent-line-to 0)
                  (just-one-space))))))))
    
    (ad-activate 'comment-dwim))

  (setq major-mode 'css-mode)
  (setq mode-name "CSS")
  (run-hooks 'css-mode-hook))

;;Other CSS Mode Add ExtraTrying

(defvar css-mode-abbrev-table nil
  "Abbreviation table used in `css-mode' buffers.")
(define-abbrev-table 'css-mode-abbrev-table ())


(defvar css-at-rule-keywords nil
  "Keywords for CSS at rules" )
(if css-at-rule-keywords nil
  (setq css-at-rule-keywords
        '("import" "media" "page" "font-face" "charset") ))

(defvar css-at-rule-table nil
  "Table for CSS at rules" )
(if css-at-rule-table nil
  (setq css-at-rule-table (make-vector 5 0))
  (mapcar (lambda (x) (intern x css-at-rule-table))
          css-at-rule-keywords ))

(defvar css-element-keywords nil
  "Common CSS elements" )
(if css-element-keywords nil
  (setq css-element-keywords 
        '("a" "address" "b" "blockquote" "body" "br" "cite"
          "code" "dir" "div" "dd" "dl" "dt" "em" "form" "h1"
          "h2" "h3" "h4" "h5" "h6" "hr" "i" "img" "kbd" "li"
          "menu" "ol" "p" "pre" "samp" "span" "strong" "table"
          "tr" "th" "td" "tt" "ul" "var" )))

(defvar css-element-table nil
  "Table for CSS elements" )
(if css-element-table nil
  (setq css-element-table (make-vector 5 0))
  (mapcar (lambda (x) (intern x css-element-table))
          css-element-keywords ))


(defvar css-property-keywords nil "CSS properties" )
(if css-property-keywords nil
  (setq css-property-keywords
'("azimuth" "background" "background-attachment" "background-color"
  "background-image" "background-position" "background-repeat" "border"
  "border-collapse" "border-color" "border-spacing" "border-style"
  "border-top" "border-right" "border-bottom" "border-left"
  "border-top-color" "border-right-color" "border-bottom-color"
  "border-left-color" "border-top-style" "border-right-style"
  "border-bottom-style" "border-left-style" "border-top-width"
  "border-right-width" "border-bottom-width" "border-left-width"
  "border-width" "bottom" "caption-side" "clear" "clip" "color"
  "content" "counter-increment" "counter-reset" "cue" "cue-after"
  "cue-before" "cursor" "direction" "display" "elevation" "empty-cells"
  "float" "font" "font-family" "font-size" "font-size-adjust"
  "font-stretch" "font-style" "font-variant" "font-weight" "height"
  "left" "letter-spacing" "line-height" "list-style" "list-style-image"
  "list-style-position" "list-style-type" "margin" "margin-top"
  "margin-right" "margin-bottom" "margin-left" "marker-offset" "marks"
  "max-height" "max-width" "min-height" "min-width" "orphans" "outline"
  "outline-color" "outline-style" "outline-width" "overflow" "padding"
  "padding-top" "padding-right" "padding-bottom" "padding-left" "page"
  "page-break-after" "page-break-before" "page-break-inside" "pause"
  "pause-after" "pause-before" "pitch" "pitch-range" "play-during"
  "position" "quotes" "richness" "right" "size" "speak" "speak-header"
  "speak-numeral" "speak-punctuation" "speech-rate" "stress"
  "table-layout" "text-align" "text-decoration" "text-indent"
  "text-shadow" "text-transform" "top" "unicode-bidi" "vertical-align"
  "visibility" "voice-family" "volume" "white-space" "widows" "width"
  "word-spacing" "z-index" )))

(defvar css-property-table nil
  "Table for CSS properties" )
(if css-property-table nil
  (setq css-property-table (make-vector 5 0))
  (mapcar (lambda (x) (intern x css-property-table))
          css-property-keywords ))



(defun css-complete-symbol (&optional table predicate prettify)
  (let* ((end (point))
	 (beg (save-excursion
		(skip-syntax-backward "w")
		(point)))
	 (pattern (buffer-substring beg end))
	 (table (or table obarray))
	 (completion (try-completion pattern table predicate)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern))
	  ((not (string-equal pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern table predicate)))
	     (if prettify
		 (setq list (funcall prettify list)))
	     (with-output-to-temp-buffer "*Help*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))



(defun css-complete-at-keyword ()
  "Complete the standard element at point"
  (interactive)
  (let ((completion-ignore-case t))
    (css-complete-symbol css-at-rule-table) ))

(defun css-complete-element ()
  "Complete the standard element at point"
  (interactive)
  (let ((completion-ignore-case t))
    (css-complete-symbol css-element-table) ))

(defun css-complete-property ()
  "Complete the standard element at point"
  (interactive)
  (let ((completion-ignore-case t))
    (css-complete-symbol css-property-table) ))


(defvar css-mode-map ()
  "Keymap used in CSS mode.")
  (setq css-mode-map (make-sparse-keymap))
  (define-key css-mode-map (read-kbd-macro "C-c C-c") 'css-insert-comment)
  (define-key css-mode-map (read-kbd-macro "C-c C-u") 'css-insert-url)
  (define-key css-mode-map "\C-c\C-a" 'css-complete-at-keyword)
  (define-key css-mode-map "\C-c\C-e" 'css-complete-element)
; (define-key css-mode-map "\C-c\C-p" 'css-complete-property)
; (define-key css-mode-map (read-kbd-macro "C-RET") 'css-complete-property)
  (define-key css-mode-map (read-kbd-macro "M-RET") 'css-complete-property)
; (define-key css-mode-map (read-kbd-macro "TAB") 'css-complete-property)


;)


;;; Typing comment and url shortcuts-from old css-mode


(define-skeleton css-insert-comment
  "Inserts a full comment." nil
  "/* " _ " */")

(define-skeleton css-insert-url
  "Inserts a URL." nil
  "url(" _ ")")



(provide 'css-mode)
;;; css-mode.el ends here
