;;; org-fireforg.el --- provide functionality for interaction of
;;;                     Fireforg, a Firefox extension, with org mode


;; Copyright 2009 Andreas Burtzlaff
;;
;; Author: Andreas Burtzlaff < andreas at burtzlaff dot de >
;; Version: 0.1alpha13
;; Keywords: org-mode firefox annotations
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Protocol:
;;
;;    fireforg-show-annotation://<file (encoded)>/<header (encoded)>
;;    ---
;;      Opens the given file in emacs and searches for header
;;
;;    fireforg-bibtex-entry://<BibTeX entry (encoded)>
;;    ---
;;      Sends a BibTeX entry that is formatted according to `org-fireforg-received-bibtex-format'
;;      and put into the kill ring

(require 'org)

(require 'org-protocol)

(require 'bibtex)

(add-to-list 'org-protocol-protocol-alist
             '("Fireforg show annotation: fireforg-show-annotation://<file (encoded)>/<header (encoded)>"
              :protocol "fireforg-show-annotation"
              :function org-fireforg-show-annotation))

(add-to-list 'org-protocol-protocol-alist
             '("Fireforg get bibtex entry: fireforg-bibtex-entry://<bibtex entry (encoded)>"
              :protocol "fireforg-bibtex-entry"
              :function org-fireforg-receive-bibtex-entry))

(defgroup org-fireforg nil
  "Options for the Fireforg extension of Org-mode."
  :group 'org)

(defcustom org-fireforg-received-bibtex-format 'heading
  "Non-nil means, transform bibtex entries.

  If the variable is `headers' the entry is transformed into a
  heading with the bibtex entries as properties prefixed with
  `BIB_'. The CUSTOM_ID is set to the bibtex key."
  :group 'org-fireforg
  :type '(choice
	  (const :tag "Create heading with properties" heading)
	  (const :tag "BibTex" nil)
          (const :tag "Create heading with properties and BibTeX entry as content" headingWithPropsAndBibTeXContent)
          (const :tag "Create heading with BibTeX entry as content" headingWithBibTeXContent)))

;; Searches for header in given file
(defun org-fireforg-show-annotation (data)
  (let* ((arguments (org-protocol-split-data data t))
         (file (nth 0 arguments))
         (heading (nth 1 arguments))
         (frameList (or (visible-frame-list) (frame-list) )))
        (find-file file)
        (goto-char (point-min))
        (re-search-forward (regexp-quote heading))
        (beginning-of-line)
        (org-show-context)
        (if frameList (select-frame-set-input-focus (car frameList)))
))


;; Renamed functions of rewritten org-registry.el
;; Temporarily moved here to avoid confusing.
;; Subject to change.


;; This needs to be customizable
(defun org-fireforg-registry-file-set () (org-agenda-files))

(defcustom org-fireforg-registry-file
  (concat (getenv "HOME") "/.org-fireforg-registry.el")
  "The Org registry file."
  :group 'org-fireforg-registry
  :type 'file)

(defcustom org-fireforg-registry-file-xml
  (concat (getenv "HOME") "/.org-fireforg-registry.xml")
  "The Org registry file in xml format. Used by fireforg."
  :group 'org-fireforg-registry
  :type 'file)

(defcustom org-fireforg-registry-find-file 'find-file-other-window
  "How to find visit files."
  :type 'function
  :group 'org-fireforg-registry)

(defvar org-fireforg-registry-alist nil
  "An alist containing the Org registry.")


;;;###autoload
(defun org-fireforg-registry-initialize (&optional from-scratch)
  "Initialize `org-fireforg-registry-alist'. 
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-fireforg-registry-file' and make it the new value for
`org-fireforg-registry-alist'."
  (interactive "P")
  ;;(message (concat "org-fireforg-registry-initialize: org-agenda-files = " (with-output-to-string (prin1 org-agenda-files)))) ;; DEBUG
  (cond ((or from-scratch (not (file-exists-p org-fireforg-registry-file)))
	  ;; create a new registry
	  (setq org-fireforg-registry-alist nil)
	  (mapc 
	   (lambda (file) 
	     (setq org-fireforg-registry-alist (org-fireforg-registry-get-entries (expand-file-name file) org-fireforg-registry-alist))) 
	   (org-fireforg-registry-file-set))
	  
;;	  (when from-scratch
	    (org-fireforg-registry-create org-fireforg-registry-alist)
	    (org-fireforg-registry-create-xml org-fireforg-registry-alist))
	(t 
	 ;; eval the registry file
	 (with-temp-buffer
	   (insert-file-contents org-fireforg-registry-file)
;;         (eval-buffer) ;; reloading the registry is not working yet. Use (org-fireforg-registry-initialize t) for the time being.
           )
         (org-fireforg-registry-create-xml org-fireforg-registry-alist))))

;;;###autoload
(defun org-fireforg-registry-insinuate ()
  "Call `org-fireforg-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit."
  (interactive)
  (add-hook 'org-mode-hook 
	    (lambda() (add-hook 'after-save-hook 
				'org-fireforg-registry-update t t))))

;; Warning: complex data structure ahead.
(defun org-fireforg-registry-get-entries (currentFile &optional registry)
  "Merge all Org links in FILE into the registry."
  (let (bufstr
        (result registry)
        (add-entry-for (function (lambda (link desc)
				   (let* ((point (match-beginning 0))
					  (onHeading (org-on-heading-p))
					  (headingPoint 0)
					  (headingTagsAndDOI 
                                           (save-excursion 
                                             (if (org-before-first-heading-p) (cons "" nil) 
                                               (org-back-to-heading t) 
                                               (setq headingPoint (point)) 
                                               (let ((ohc (org-heading-components)))
                                                 (list (nth 4 ohc) (org-get-tags-at) (org-entry-get headingPoint "BIB_doi"))))))
					  (heading (car headingTagsAndDOI))
					  (tags (nth 1 headingTagsAndDOI))
                                          (doi (if (nth 2 headingTagsAndDOI) (org-fireforg-bibtex-trim-string (nth 2 headingTagsAndDOI))))
					  (contentEntry (list point desc onHeading))
					  (headingEntry (list heading headingPoint onHeading tags (list contentEntry) doi))
					  (fileEntry (list (expand-file-name currentFile) (list headingEntry)))
					  (linkEntry (list link (list fileEntry)))
					  (existingLinkEntry (assoc link result))
					  (existingFileEntry (assoc (expand-file-name currentFile) (nth 1 existingLinkEntry)))
					  (existingHeadingEntry (assoc heading (nth 1 existingFileEntry))))

				     (cond (existingLinkEntry
					    (cond (existingFileEntry
						   (cond (existingHeadingEntry (setf (nth 4 existingHeadingEntry) (cons contentEntry (nth 4 existingHeadingEntry))))
							 (t (setf (nth 1 existingFileEntry) (cons headingEntry (nth 1 existingFileEntry))))))
						  (t (setf (nth 1 existingLinkEntry) (cons fileEntry (nth 1 existingLinkEntry))))))
					   (t (add-to-list 'result linkEntry))))))

		       ))

    (with-temp-buffer
      (insert-file-contents currentFile)
      ;; Turn on org-mode in order to use org-heading-components and org-get-tags-at
      ;; This can have severe impact on performance for large files, so I want to get rid of this requirement.
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
        (funcall add-entry-for (match-string-no-properties 1) (or (match-string-no-properties 3) "No description")))
      (goto-char (point-min))
      (while (re-search-forward org-angle-link-re nil t)
        (funcall add-entry-for (concat (match-string-no-properties 1) ":" (match-string-no-properties 2)) "" ))
      (goto-char (point-min))
      (while (re-search-forward org-plain-link-re nil t)
        (funcall add-entry-for (match-string-no-properties 0) "" ))
      (goto-char (point-min))
      ;; add all DOI's in properties as urls with prefix "http://dx.doi.org/"
      (org-map-entries
       (lambda () 
         (let ((doi (org-entry-get (point) "BIB_doi")))
           ;;(message (concat "current file:" currentFile))
           (if doi 
               (funcall add-entry-for 
                        ;; link
                        ;; Due to a bug in Zotero it might happen that
                        ;; the doi identifier is enclosed in two sets
                        ;; of "{}" brackets.
                        ;; Therefore the function org-fireforg-bibtex-trim-string is apply two times.
                        (org-fireforg-doi-to-url (org-fireforg-bibtex-trim-string (org-fireforg-bibtex-trim-string doi)))
                        ;; description
                        (nth 4 (org-heading-components)))))))
      result)))

;;;###autoload
(defun org-fireforg-registry-update ()
  "Update the registry for the current Org file, if it is in org-fireforg-registry-file-set."
  (interactive)
  (unless (org-mode-p) (error "Not in org-mode"))
  (cond ((not (file-exists-p org-fireforg-registry-file))
         ;; registry-file doesn't exist -> create it from scratch
         (org-fireforg-registry-initialize t))
        (t
	 ;; update existing registry-file
	 (let ((from-file (expand-file-name (buffer-file-name))))
	   (cond ((member from-file (mapcar 'expand-file-name (org-fireforg-registry-file-set)))
		  (let ((registryFiltered (org-fireforg-registry-filter-where-filename-not from-file org-fireforg-registry-alist)))
		    (setq org-fireforg-registry-alist (org-fireforg-registry-get-entries from-file registryFiltered))
                    (org-fireforg-registry-create org-fireforg-registry-alist)
                    (org-fireforg-registry-create-xml org-fireforg-registry-alist)
                    ;;(message (format "Org registry updated for %s. Found %i entries. Registry contains %i entries." (file-name-nondirectory from-file) (length new-entries) (length org-fireforg-registry-alist)))
                    ))
                 ;;		 (t (message (format "Current file %s is not in org-fireforg-registry-file-set." from-file))) 
                 )))))

;; requires expanded filename as argument
(defun org-fireforg-registry-filter-where-filename-not (filename registry)
  (cond ((not registry) nil)
        ((nlistp registry) (error "org-fireforg-registry-filter-where-filename-not: argument registry is not a list"))
        (t
         (mapcar (lambda (linkEntry) 
                   (list (car linkEntry) 
                         (org-fireforg-registry-filter 
                          (lambda (fileEntry) 
                            (not (string= (expand-file-name (car fileEntry)) filename)))
                          (nth 1 linkEntry))))
                 registry))))


(defun org-fireforg-registry-create (entries)
  "Create `org-fireforg-registry-file' with ENTRIES."
  (let (entry)
    (with-temp-buffer
      (find-file org-fireforg-registry-file)
      (erase-buffer)
      (insert
         (concat ";; -*- emacs-lisp -*-\n"
	         ";; Org registry\n"
	         ";; You shouldn't try to modify this buffer manually\n\n"
	         "(setq org-fireforg-registry-alist\n"
                 (org-fireforg-registry-to-string-rec org-fireforg-registry-alist) ")"))
	 
      (set-buffer-file-coding-system 'utf-8)
      (save-buffer)
      (kill-buffer (current-buffer))))
  (message "Org registry created"))

(defun org-fireforg-registry-to-string-rec (obj)
  "Return elisp code that generates an object with identical content as the argument"
  (cond ((not obj) "nil")
        ((stringp obj) (concat "\"" obj "\"")) 
        ((nlistp obj) (with-output-to-string (prin1 obj)))
        (t ;; obj is a list
          (concat "(list " (mapconcat 'org-fireforg-registry-to-string-rec obj " ") ")")
         )
  )
)

(defun org-fireforg-registry-create-xml (entries)
  "Create org-fireforg-registry-file-xml with ENTRIES in xml format."
  (with-temp-buffer
    (insert 
     (concat "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\" ?>\n<orgregistry>\n" 
	     (reduce 'concat (mapcar (lambda (linkEntry)
				       (concat "<link url=\"" (url-insert-entities-in-string (nth 0 linkEntry)) "\">\n"
					       (reduce 'concat (mapcar (lambda (fileEntry) 
									 (let ((file (nth 0 fileEntry)))
									   (reduce 'concat (mapcar (lambda (headingEntry) 
												     (concat " <heading file=\"" (url-insert-entities-in-string file) "\" "
													     "text=\""          (url-insert-entities-in-string (nth 0 headingEntry)) "\" "
													     "point=\""         (number-to-string (nth 1 headingEntry)) "\" "
													     "linkInHeading=\"" (url-insert-entities-in-string (if (nth 2 headingEntry) "t" "f")) "\" "
													     "tags=\""          (if (nth 3 headingEntry)
                                                                                                                                    (url-insert-entities-in-string (concat ":" (mapconcat 'identity (nth 3 headingEntry) ":") ":"))
                                                                                                                                    "") "\" "
                                                                                                             (if (nth 5 headingEntry) (concat "doi=\"" (nth 5 headingEntry) "\"  ") "")
													     ">\n  "
													     (reduce 'concat (mapcar (lambda (contentEntry) 
																       (concat "  <contentEntry point=\"" (number-to-string (nth 0 contentEntry)) "\" "
																	       "description=\"" (url-insert-entities-in-string (nth 1 contentEntry)) "\" "
																	       "inHeading=\"" (if (nth 2 contentEntry) "t" "f") "\"/>\n"))
																     (nth 4 headingEntry)) :initial-value "")
													     " </heading>"))
												   (nth 1 fileEntry)) :initial-value "")
									   )
									 ) (nth 1 linkEntry)) :initial-value "")
					       "</link>\n")) entries) :initial-value "")
	     "</orgregistry>"))

    (when (file-writable-p org-fireforg-registry-file-xml)
      (set-buffer-file-coding-system 'utf-8)
      (write-region (point-min)
		    (point-max)
		    org-fireforg-registry-file-xml))))

(defun org-fireforg-registry-filter (condp lst)
  (delq nil
	  (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun org-fireforg-receive-bibtex-entry (data)
  ;;(message "Received bibtex string") ;; DEBUG
  (let* ((arguments (org-protocol-split-data data t))
         (bibtex (nth 0 arguments))
         (bibtexParsed (org-fireforg-parse-bibtex-entry-wrapper bibtex)))
    (kill-new (cond ((eq org-fireforg-received-bibtex-format 'heading)
                     (concat (org-fireforg-generate-heading bibtexParsed) "\n" (org-fireforg-bibtex-entry-to-properties bibtexParsed)))
                    ((eq org-fireforg-received-bibtex-format 'headingWithPropsAndBibTeXContent)
                     (concat (org-fireforg-generate-heading bibtexParsed) "\n" (org-fireforg-bibtex-entry-to-properties bibtexParsed) bibtex "\n"))
                    ((eq org-fireforg-received-bibtex-format 'headingWithBibTeXContent)
                     (concat (org-fireforg-generate-heading bibtexParsed) "\n" bibtex "\n"))
                    ((not org-fireforg-received-bibtex-format) bibtex)))
    (message "Saved BibTeX entry to kill ring.") 
    )
  nil)

(defun org-fireforg-parse-bibtex-entry-wrapper (bibtexEntryString)
  (with-temp-buffer
    (insert bibtexEntryString)
    (goto-char (point-min))
    ;;(bibtex-next-field t)
    (re-search-forward "@")
    (goto-char (match-beginning 0))
    (bibtex-parse-entry)))

(defun org-fireforg-bibtex-trim-string (string)
  (replace-regexp-in-string "[\"}] *$" "" (replace-regexp-in-string "^ *[{\"]" "" string)))

(defun org-fireforg-headings-to-bibtex (&optional match)
  (reduce 'concat (org-map-entries (lambda () (concat (org-fireforg-heading-to-bibtex-entry) "\n\n")) match )))

(defun org-fireforg-heading-to-bibtex-entry ()
  (let* ((properties (org-entry-properties))
         (type (cdr (assoc "BIB_entryType" properties)))
         (id (cdr (assoc "CUSTOM_ID" properties)))
         (properties (rassq-delete-all id (rassq-delete-all type properties))))
    (cond ((and type id)
           (concat "@" type "{" id 
                   (reduce 'concat 
                           (mapcar (lambda (prop) (if (and (> (length (car prop)) 4) (string= (substring (car prop) 0 4) "BIB_")) (concat ",\n  " (substring (car prop) 4) " = " (cdr prop) ))) properties ) :initial-value "") "\n}")))))

(defun org-fireforg-bibtex-entry-to-properties (bibtexEntry)
  (concat ":PROPERTIES:\n"
   (reduce 'concat (mapcar (lambda (entry) 
     (concat (cond ((string= (car entry) "=key=") "  :CUSTOM_ID")
                   ((string= (car entry) "=type=") "  :BIB_entryType")
                   (t (concat "  :BIB_" (car entry) ))) ": " (cdr entry) "\n")) bibtexEntry) :initial-value "")
   ":END:\n"))

(defun org-fireforg-generate-heading (bibtexEntry)
  (let ((heading (concat "* [[" (org-fireforg-bibtex-trim-string (cdr (assoc "url" bibtexEntry))) "][" (org-fireforg-bibtex-trim-string (cdr (assoc "title" bibtexEntry))) "]]" )))
    ;;(with-temp-buffer (insert heading) (goto-char (point-min)) (org-id-get-create) (buffer-substring (point-min) (point-max)))
    heading
    ))

;; exports the bibtex properties of the current buffer to selectable file
(defun org-fireforg-export-bibtex-to-file (file)
  (interactive "F")
  (save-excursion 
    (cond ((not file) (error "No file supplied"))
          ((let ((bibtex (org-fireforg-headings-to-bibtex)))
             (with-temp-buffer 
             (insert bibtex)
             (write-file file t)))))))

(defun org-fireforg-export-bibtex-to-new-buffer ()
  (interactive)
  (let ((bibtex (org-fireforg-headings-to-bibtex))
        ;; find nearest bibliography entry before point
        (prevId (save-excursion 
                  (while (and (not (org-before-first-heading-p)) (if (org-at-heading-p) (or (null (org-entry-get (point) "CUSTOM_ID")) (string= (org-entry-get (point) "CUSTOM_ID") "")) 1) (not (bobp)) ) (progn (backward-char) (org-back-to-heading t) ) )
                  (if (or (org-before-first-heading-p) (bobp)) nil (org-entry-get (point) "CUSTOM_ID")))))
    (switch-to-buffer (generate-new-buffer "*BibTeX export*"))
    (insert bibtex)
    (goto-char (point-min))
    (if prevId (progn (re-search-forward (regexp-quote prevId)) (beginning-of-line)))
    (bibtex-mode)))

(defun org-fireforg-doi-to-url (string)
       (concat "http://dx.doi.org/" 
 (replace-regexp-in-string " " "%20"
 (replace-regexp-in-string "#" "%23"
 (replace-regexp-in-string "\"" "%22" 
 (replace-regexp-in-string "%" "%25" string))))))

(provide 'org-fireforg)
