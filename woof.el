
;;; woof.el --- Emacs interface to exchange files on different computers via woof

;; Copyright (C) 2007 by Stefan Reichoer

;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; woof.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; woof.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The problem:
;; You work on different computers and you want to send files or
;; the selected text to other hosts.

;; The solution:
;; woof (Web Offer One File): http://www.home.unix-ag.org/simon/woof.html
;; woof is a small webserver that serves a given file
;; you can download the file on a different host from this webserver

;; woof.el integrates the woof script in your emacs work-flow:
;; You can easily provide a file to download (M-x woof-provide-file)
;; And it is also easy to receive such a file on a different host
;; (M-x woof-receive or C-u M-x woof-receive)

;; woof.el needs the woof script from:
;; http://www.home.unix-ag.org/simon/woof

;; woof.el also needs wget
;; It should work on GNU/Linux systems, I have also added some hacks to make it work on cygwin.

;; The latest version of woof.el can be found at:
;; http://www.xsteve.at/prg/emacs/

;; Usage:
;; put the following in your .emacs:
;; (require 'woof)

;; (woof-set-download-url-list '("http://xxx.xxx.xxx.xxx:8080" "http://yyy.yyy.yyy.yyy:8080"
;;                               ("nice-name" "http://zzz.zzz.zzz.zzz:8080")))
;; (global-set-key [<your key here>] 'woof-provide-dwim)
;; (global-set-key [<your key here>] 'woof-receive)

;; If you are using cygwin, you probably need the following also:
;; (setq woof-wget-cmd "c:/cygwin/bin/wget")
;; (setq woof-cmd "c:/cygwin/usr/local/bin/woof")

;; Woof is also available via the Tools menu

;; Please contact me, if you find woof.el useful, or if you have ideas for improvements.

;;; History:
;;

;;; Code:

(defvar woof-cmd "woof")
(defvar woof-wget-cmd "wget")

(defvar woof-download-url-list nil)

(defvar woof-completing-read-function (if (fboundp 'ido-completing-read) 'ido-completing-read 'completing-read))

(defvar woof-use-cygwin (eq system-type 'windows-nt))

(defvar woof-temp-dir
  (expand-file-name
   (or
    (when (boundp 'temporary-file-directory) temporary-file-directory)
    (when (fboundp 'temp-directory) (file-name-as-directory (temp-directory)))
    "/tmp/")) "The directory that is used to store temporary files for woof.")

;;; Internal variables
(defvar woof-process-buffer-name "*woof*")
(defvar woof-wget-process-buffer-name "*woof-wget*")
(defvar woof-clip-file-name "woof.el-Clipboard")

(defvar woof-download-url nil)
(defvar woof-current-download-url nil)

(defvar woof-last-provided-file nil)
(defvar woof-delete-last-provided-file nil)
(defvar woof-last-provided-message nil)

;; compatibility
(defun woof-mark-active()
  (if (boundp 'region-active-p)
      (region-active-p) ;; XEmacs
    mark-active)) ;; GNU Emacs

(defun woof-add-to-global-menu ()
  "Add a woof menu to the Tools main menu"
  (interactive)
  (condition-case nil
      (easy-menu-add-item nil '("tools")
                          (append '("Woof")
                                  '(["Provide DWIM" 'woof-provide-dwim t]
                                    ["Provide the woof script" 'woof-provide-woof t]
                                    ["Provide woof.el" 'woof-provide-woof-el t]
                                    ["Cancel woof service" 'woof-provide-cancel t])
                                  '("---")
                                  (mapcar '(lambda (url) (vector (format "Receive from %s" (if (consp url) (format "%s: %s" (car url) (cadr url)) url))
                                                                 `(lambda () (interactive) (woof-receive-from (quote ,url)) t)))
                                          woof-download-url-list)
                                  ))
    (error (message "woof: could not install menu"))))

(defun woof-provide-dwim ()
  "When mark is active, send the selected region, otherwise the current file.
When woof is still serving a file, ask if this server should be killed."
  (interactive)
  (when woof-last-provided-message
    (when (y-or-n-p (format "woof is still serving '%s' - kill this server? " woof-last-provided-message))
      (woof-provide-cancel)
      (sit-for 0.5)))
  (cond ((woof-mark-active)
         (woof-provide-clip (buffer-substring-no-properties (region-beginning) (region-end))))
        ((member major-mode '(dired-mode locate-mode))
         (woof-provide-file (dired-get-filename)))
        ((eq major-mode 'gnus-article-mode)
         (woof-provide-gnus-attachment))
        (t
         (save-buffer)
         (woof-provide-file (buffer-file-name)))))

(defun woof-fixup-cygwin-path (file-name)
  (replace-regexp-in-string "c:/" "/cygdrive/c/" file-name))

(defvar woof-current-ip nil)
(defun woof-get-ip-on-windows ()
  (unless woof-current-ip
    (with-temp-buffer
      (shell-command "ipconfig" (current-buffer))
      (goto-char (point-min))
      (re-search-forward "IP-.+: \\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)")
      (setq woof-current-ip (match-string 1))))
  woof-current-ip)

(defun woof-provide-file (file-name)
  "Provide a file to download via woof."
  (interactive "fProvide File via woof: ")
  (let ((delete-provided-file nil)
        (full-cmd
         (if woof-use-cygwin
             (progn
               (when (numberp (string-match " " file-name))
                 (copy-file file-name (concat woof-temp-dir (file-name-nondirectory file-name)) t)
                 (setq file-name (concat woof-temp-dir (file-name-nondirectory file-name)))
                 (setq delete-provided-file t))
               (let* ((cygwin-bin-path "c:/cygwin/bin")
                      (sh-cmd (format "%s/sh -c" cygwin-bin-path))
                      (python-cmd (format "%s/python" cygwin-bin-path))
                      (cyg-file-name (woof-fixup-cygwin-path file-name))
                      (cmd (format "%s %s" sh-cmd (shell-quote-argument
                                                   (format "%s %s %s" python-cmd woof-cmd cyg-file-name)))))
                 cmd))
           (format "%s %s" woof-cmd (shell-quote-argument file-name)))))
    (let ((woof-proc (start-process-shell-command "woof" woof-process-buffer-name full-cmd)))
      (setq woof-last-provided-file (expand-file-name file-name))
      (setq woof-delete-last-provided-file delete-provided-file)
      (setq woof-last-provided-message file-name)
      (set-process-sentinel woof-proc 'woof-process-sentinel)
      (set-process-filter woof-proc 'woof-process-filter))
    (when woof-use-cygwin
        (message "woof: Now serving '%s' on '%s'" file-name (woof-get-ip-on-windows)))))

(defun woof-provide-clip (clip-text)
  "Provide a string to download via woof.
This string is saved in a file named `woof-clip-file-name'.
If the receiver also uses woof.el, the received data is immediately copied to the kill-ring."
  (interactive "sProvide string via woof: ")
  (let ((clip-file-name (concat woof-temp-dir woof-clip-file-name)))
    (with-current-buffer (find-file-noselect clip-file-name)
      (delete-region (point-min) (point-max))
      (insert clip-text)
      (save-buffer)
      (kill-buffer (current-buffer)))
    (woof-provide-file clip-file-name)
    (setq woof-delete-last-provided-file t)
    (setq woof-last-provided-message "<clip>")))

(defun woof-provide-woof ()
  "Provide the woof script to download via woof."
  (interactive)
  (let ((full-cmd
         (if woof-use-cygwin
             (let* ((cygwin-bin-path "c:/cygwin/bin")
                    (sh-cmd (format "%s/sh -c" cygwin-bin-path))
                    (python-cmd (format "%s/python" cygwin-bin-path))
                    (cmd (format "%s %s" sh-cmd (shell-quote-argument
                                                 (format "%s %s" python-cmd woof-cmd)))))
               cmd)
           (format "%s -s" woof-cmd))))
    (let ((woof-proc (start-process-shell-command "woof" woof-process-buffer-name full-cmd)))
      (setq woof-last-provided-file 'woof)
      (setq woof-last-provided-message "woof")
      (set-process-sentinel woof-proc 'woof-process-sentinel)
      (set-process-filter woof-proc 'woof-process-filter))))

(defun woof-provide-woof-el ()
  "Provide woof.el to download via woof."
  (interactive)
  (woof-provide-file (locate-library "woof.el")))

(defun woof-provide-gnus-attachment ()
  "Provide the attachment at point from a gnus article buffer"
  (let ((handle (get-text-property (point) 'gnus-data))
        (file-name))
    (when handle
      (setq filename (or (mail-content-type-get
                          (mm-handle-disposition handle) 'filename)
                         (mail-content-type-get
                          (mm-handle-type handle) 'name)))
      (when filename
        (setq filename (concat woof-temp-dir
                               (gnus-map-function mm-file-name-rewrite-functions
                                                  (file-name-nondirectory filename))))
        (mm-save-part-to-file handle filename)
        (woof-provide-file filename)
        (setq woof-delete-last-provided-file t)
        (setq woof-last-provided-message (format "Gnus Attachment <%s>" (file-name-nondirectory filename)))))))

(defun woof-provide-cancel ()
  "Cancel the serving of a provided file"
  (interactive)
  (kill-buffer (get-buffer woof-process-buffer-name)))

(defun woof-process-filter (process str)
  (save-window-excursion
    (set-buffer woof-process-buffer-name)
    (let ((start-position (point-max)))
      ;;(message "woof-process-filter: %s" str)
      (goto-char start-position)
      (insert str)
      (save-excursion
        (goto-char start-position)
        (when (looking-at "Now serving on \\(.+\\)")
          (message "woof: Now serving '%s' on '%s'" woof-last-provided-message (match-string 1))
          (setq woof-provide-url (match-string 1)))))))

(defun woof-process-sentinel (process event)
  ;;(princ (format "Process: %s had the event `%s'" process event))
  (cond ((string= event "finished\n")
         (when woof-delete-last-provided-file
             (when (file-exists-p woof-last-provided-file)
               (delete-file woof-last-provided-file)))
         (if (string= (file-name-nondirectory woof-last-provided-file)
                      (file-name-nondirectory woof-clip-file-name))
             (message "woof: serving region content done")
           (message "woof: serving '%s' finished" woof-last-provided-message)))
        ((string= event "killed\n")
         (message "woof process killed"))
        ((string= event "hangup\n")
         (message "woof process hangup"))
        ((string-match "exited abnormally" event)
         (while (accept-process-output process 0 100))
         ;; find last error message and show it.
         (goto-char (point-max))
         (message "woof failed: %s" (replace-regexp-in-string "\n" "" event)))
        (t
         (message "woof process had unknown event: %s" event)))
    (setq woof-last-provided-message nil))

;; ------------------------------------------------------------------------------------------
;; woof wget download interface
;; ------------------------------------------------------------------------------------------

(defun woof-select-dowload-url ()
  "Select the default download Url for `woof-receive' calls."
  (interactive)
  (let ((selected (funcall woof-completing-read-function "Select woof download Url: " woof-download-url-list)))
    (setq woof-download-url (or (assoc selected woof-download-url-list) selected))))

(defun woof-receive-from (url)
  "Download using `woof-receive'. Use URL as address."
  (let ((woof-download-url url))
    (woof-receive nil)))

(defun woof-receive (arg)
  "Download via wget from the given url in `woof-download-url'.
When called with a prefix argument, select the download url first via `woof-select-dowload-url'."
  (interactive "P")
  (when arg
    (woof-select-dowload-url))
  (setq woof-current-download-url (or woof-download-url (car woof-download-url-list)))
  (when (consp woof-current-download-url)
    (setq woof-current-download-url (cadr woof-current-download-url)))
  (woof-wget-run (format "%s -nv %s" woof-wget-cmd woof-current-download-url)))

(defun woof-wget-run (cmd)
  (setq woof-download-file nil)
  (let ((woof-proc (start-process-shell-command "woof-wget" woof-wget-process-buffer-name cmd))
        (directory default-directory))
    (with-current-buffer woof-wget-process-buffer-name
      (setq default-directory directory)) ;;seems to be needed on windows
    (set-process-sentinel woof-proc 'woof-wget-process-sentinel)
    (set-process-filter woof-proc 'woof-wget-process-filter)))

(defun woof-wget-process-filter (process str)
  (save-window-excursion
    (set-buffer woof-wget-process-buffer-name)
    (let ((start-position (point-max)))
      ;; (message "woof-wget-process-filter: %s" str)
      (goto-char start-position)
      (setq start-position (line-beginning-position))
      (insert str)
      (save-excursion
        (goto-char start-position)
        ;; 10:42:26 URL:http://123.45.67.89:8080/woof.el [4124/4124] -> "woof.el" [1]
        (when (looking-at ".+ -> \"\\(.+\\)\"")
          (setq woof-download-file (expand-file-name (match-string 1))))))))

(defun woof-wget-process-sentinel (process event)
  ;;(princ (format "Process: %s had the event `%s'" process event))
  (cond ((string= event "finished\n")
         (if (string= (file-name-nondirectory woof-download-file)
                      (file-name-nondirectory woof-clip-file-name))
             (with-current-buffer (find-file-noselect woof-download-file)
               (kill-new (buffer-substring-no-properties (point-min) (point-max)))
               (kill-buffer (current-buffer))
               (delete-file woof-download-file)
               (message "woof: Copied received data to clipboard"))
           (message "woof-wget downloaded: %s" woof-download-file)))
        ((string= event "killed\n")
         (message "woof-wget process killed"))
        ((string= event "hangup\n")
         (message "woof-wget process hangup"))
        ((string-match "exited abnormally" event)
         (message "woof-wget failed: %s, %s probably not serving"
                  (replace-regexp-in-string "\n" "" event) woof-current-download-url))
        (t
         (message "woof-wget process had unknown event: %s" event))))

(defun woof-set-download-url-list (l)
  "Set `woof-download-url-list' to L and update the woof menu entry"
  (setq woof-download-url-list l)
  (woof-add-to-global-menu))

;; Runtime initialization
(woof-add-to-global-menu)


(provide 'woof)

;;; arch-tag: f3b53108-383c-49a7-a70f-3c8a7fa6b27c
;;; woof.el ends here
