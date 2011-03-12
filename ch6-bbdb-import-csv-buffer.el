;;; ch6-bbdb-import-csv-buffer.el --- 
;; 
;; Filename: ch6-bbdb-import-csv-buffer.el
;; Description: 
;; Author: Sacha Chua
;; Maintainer: Martial Boniou
;; Created: Tue Apr 15 14:55:41 2008
;; Version: 
;; Last-Updated: Tue Apr 15 15:02:55 2008 (7200 CEST)
;;           By: Martial Boniou
;;     Update #: 3
;; URL: 
;; Keywords: CSV, outlook, mail, bbdb, gnus, vm, wl
;; Compatibility: http://ulf.epplejasper.de/downloads staffs
;; 
;; Features that might be required by this library:
;;
;;   Required feature `ch6-bbdb-import-csv-buffer' was not provided.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: Outlook-style CSV importer for BBDB, requires csv.el 
;;;             and lookout.el
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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

(require 'lookout)
(defconst wicked/lookout-bbdb-mapping-table-outlook
  '(("name" "Name")
    ("net" "E-mail Address")
    ("notes" "Notes")
    ("phones" "Mobile Phone"
     "Home Phone"
     "Home Phone 2"
     "Home Fax"
     "Business Phone"
     "Business Phone 2"
     "Business Fax"
     "Other Phone"
     "Other Fax")
    ("addr1" "Home Address")
    ("addr2" "Business Address")
    ("addr3" "Other Address")
    ("lastname" "Last Name")
    ("firstname" "First Name")
    ("job" "Job Title")
    ("company" "Company")
    ("otherfields" ""))
  "Field mappings for Outlook-type CSVs exported from Outlook, Gmail, LinkedIn, etc.")

(defun wicked/bbdb-import-csv-line (line)
  "Import LINE as a CSV, trying to merge it with existing records."
  (let* (record
	 (name  (lookout-bbdb-get-value "name" line))
	 (lastname (lookout-bbdb-get-value "lastname" line))
	 (firstname (lookout-bbdb-get-value "firstname" line))
	 (company   (lookout-bbdb-get-value "company" line))
         (job       (lookout-bbdb-get-value "job" line))
	 (net       (lookout-bbdb-get-value "net" line))
	 (addr1     (lookout-bbdb-get-value "addr1" line))
	 (addr2     (lookout-bbdb-get-value "addr2" line))
	 (addr3     (lookout-bbdb-get-value "addr3" line))
	 (phones    (lookout-bbdb-get-value "phones" line t)) ;; !
	 (notes     (lookout-bbdb-get-value "notes" line ))
         (j (concat job ", " company))
	 (otherfields (lookout-bbdb-get-value "otherfields" line t))
	 (addrs nil)
         name-search
	 (message ""))
    (if (string= company "") (setq company nil))
    (if (string= notes "") (setq notes nil))
    (if (string= name "") (setq name nil))
    (setq name-search (concat "^" (or name (concat firstname " " lastname))))
    (setq record (or (bbdb-search (bbdb-records) nil nil net)
		     (bbdb-search (bbdb-records) name-search)))
    (if record
	(progn
	  ;; Matching records found, update first matching record
	  (setq record (car record))
	  (let ((nets (bbdb-record-net record)))
	    (unless (member net nets)
	      ;; New e-mail address noticed, add to front of list
	      (add-to-list 'nets net)
	      (bbdb-record-set-net record nets)
	      (message "%s: New e-mail address noticed: %s"
		       (or name (concat firstname " " lastname)) net)))
	  ;; Check if job title and company have changed
	  (when (or job company)
	    (cond
	     ((string= (or (bbdb-record-company record) "") "")
	      (bbdb-record-set-company record j))
	     ((string= (bbdb-record-company record) j)
	      nil)
	     (t
	      (bbdb-record-set-notes
	       record
	       (concat "Noticed change from job title of "
		       (bbdb-record-company record)
		       "\n"
		       (bbdb-record-notes record)))
	      (message "%s: Noticed change from job title of %s to %s"
		       (or name (concat firstname " " lastname))
		       (bbdb-record-company record) j)
	      (bbdb-record-set-company record j)))))
      ;; No record found, create record
      (if (and addr1 (> (length addr1) 0))
	  (add-to-list 'addrs
		       (vector "Address 1" (list addr1) "" "" "" "")))
      (if (and addr2 (> (length addr2) 0))
	  (add-to-list 'addrs
		       (vector "Address 2" (list addr2) "" "" "" "")))
      (if (and addr3 (> (length addr3) 0))
	  (add-to-list 'addrs
		       (vector "Address 3" (list addr3) "" "" "" "")))
      (setq record (list
		    (wicked/lookout-bbdb-create-entry
		     (or name (concat firstname " " lastname))
		     (concat job ", " company)
		     net
		     addrs
		     phones
		     notes
		     otherfields))))
    record))

(defun wicked/lookout-bbdb-create-entry (name company net addrs phones notes
					      &optional otherfields)
  (when (or t (y-or-n-p (format "Add %s to bbdb? " name)))
    ;;(message "Adding record to bbdb: %s" name)
    (let ((record (bbdb-create-internal name company net addrs phones notes)))
      (unless record (error "Error creating bbdb record"))
      (mapcar (lambda (i)
		(let ((field (make-symbol (aref i 0)))
		      (value (aref i 1)))
		  (when (and value (not (string= "" value)))
		    (bbdb-insert-new-field record field value))))
	      otherfields)
      record)))

(defun wicked/bbdb-import-csv-buffer ()
  "Import this buffer."
  (interactive)
  (let ((lookout-bbdb-mapping-table
	 wicked/lookout-bbdb-mapping-table-outlook))
    (bbdb-display-records
     (mapcar
      'wicked/bbdb-import-csv-line
      (csv-parse-buffer t)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ch6-bbdb-import-csv-buffer.el ends here
