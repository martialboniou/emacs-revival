;;; bbdb-rf.el --- some of my own addons to BBDB not in its bits
;; 
;; Copyright (C) 2002-2006 Robert Widhopf-Fenk
;;
;; Author:      Robert Widhopf-Fenk
;; Status:      Tested with VM 7.19, BBDB 2.35 & XEmacs 21.4.19
;; Keywords:    BBBD, VM, XEmacs
;; X-URL:       http://www.robf.de/Hacking/elisp
;; X-RCS:       $Id: bbdb-rf.el,v 1.26 2007-01-17 23:19:03 widhopf Exp $

;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:
;;
;; Put this file into your load path and add the following line to your ~/.vm
;; or BBDB startup file or ~/.emacs resp.  ~/.xemacs/init.el ...
;;
;; (require 'bbdb-rf)
;;
;;; History
;;
;; $Log: bbdb-rf.el,v $
;; Revision 1.26  2007-01-17 23:19:03  widhopf
;; Fixed compile time warnings for GNU Emacs 22.
;;
;; Revision 1.25  2007-01-17 23:02:51  fenk
;; Fixed path in the documentaion, i.e. made them absolute.
;;
;; Revision 1.24  2006-08-29 22:08:18  fenk
;; Applied patch from Adrian Aichner to avoid call to unbound functions, adds
;; timestamp handling to bbdb-csv-export, ...
;;
;; Added docs to `bbdb-snarf-format'.
;;
;; Revision 1.23  2006-08-29 21:35:50  fenk
;; Default of bbdb/vm-pop-up-bbdb-buffer-on-vm-system-state is t now.
;;
;; Revision 1.22  2006-06-13 23:29:43  fenk
;; Moved code from other places to this file, i.e. bbdb-snarf-format,
;; bbdb/vm-pop-up-bbdb-buffer-on-vm-system-state, bbdb-yank-ccc, ...
;;
;; Revision 1.21  2005/05/17 20:06:49  fenk
;; Thunderbird export added.
;;
;; Revision 1.20  2005/01/07 22:53:09  fenk
;; - M-x checkdoc is your friend
;; - Bugfix: Message-ID: <16856.19238.26000.448956@comcast.net>
;;   Use replace-in-string which is not known by GNU Emacs =>
;;   fixed by prepending bbdb- as in the other occurrences (we had this problem
;;   before.
;; - Enhanced vcard export a bit.
;;
;; Revision 1.19  2004/05/18 21:18:51  fenk
;; 
;; Revision 1.18  2004/05/14 09:32:50  fenk
;; Fixed URLS to my homepage.
;;
;; Revision 1.17  2004/04/29 21:16:18  fenk
;; - allow selections of arbitraty phone fields for an CVS field
;; - in order to get a "\" in GNU Emacs we need to write "\\\\" in the
;;   replacement string, whihc is not required for XEmacs. ;c(
;;
;; Revision 1.16  2004/01/19 22:33:14  fenk
;; Enhanced vcard export
;;
;; Revision 1.15  2003/10/12 09:07:55  fenk
;; - Handle undefined fields in `bbdb-csv-export' gracefully if they are not
;;   defined in the local version of the users BBDB.
;; - Added an VCARD export.
;;
;; Revision 1.14  2003/08/13 11:43:09  fenk
;; - Created a customize group bbdb-rf
;; - Added & fixed all missing & wrong defcustoms
;;
;; Revision 1.13  2003/08/13 10:56:17  fenk
;; - Fixed some comments, copyright and names.
;; - Reverted format for email and mail-alias to have just one arg, the
;;   position.
;; - Enhanced CSV-export (thanks to Sridhar Boovaragh)
;; 


;;; History:
;; 

(require 'bbdb)
(require 'bbdb-snarf)
(require 'bbdb-print)
(require 'bbdb-autoloads)
(require 'bbdb-vm)
(require 'vm)
(require 'message)

;;; Code:
(defgroup bbdb-rf nil
  "Customize bbdb-rf.el"
  :group 'bbdb)

;;;###autoload
(defcustom bbdb/sms-gateway "sms.web.de"
  "A regexp matching folder names of primary VM inboxes."
  :group 'bbdb-rf
  :type 'string)

;;;###autoload
(defcustom bbdb/sms-headers
  ;; bbdb-replace-in-string is not available during autoload
  ;; generation.
  (if (fboundp 'bbdb-replace-in-string)
      (list
       (list "From" (concat (bbdb-replace-in-string (user-full-name) " " ".")
                            "@web.de"))
       (list "Subject" (concat (bbdb-replace-in-string (user-full-name) " " ".")
                               "@web.de"))
       '("Organization" nil)))
  "A list of (header value) pairs.
It replaces existing headers in the message buffer.
If value is nil the header will be removed."
  :group 'bbdb-rf
  :type 'sexp)

;;;###autoload
(defcustom bbdb/sms-mobile-field "mobile"
  "The label of the phones containing the mobile number."
  :group 'bbdb-rf
  :type 'string)

;;;###autoload
(defcustom bbdb/sms-max-chars 160
  "The maximum number of characters of a SMS."
  :group 'bbdb-rf
  :type 'integer)

(defun bbdb-make-sms-address (record)
  "Built an email address from the mobile field of RECORD and the gateway."
  (let (m)
    (if (setq m (assoc bbdb/sms-mobile-field
                       (mapcar (lambda (p) (append p nil))
                               (bbdb-record-phones record))))
        (setq m (cadr m)
              m (bbdb-replace-in-string m "+" "00")
              m (bbdb-replace-in-string m "/" "")
              m (concat
                 (bbdb-record-name record)
                 " <" m "@" bbdb/sms-gateway ">")))))

;;;###autoload
(defun bbdb-send-sms (records &optional subject)
  "Compose a SMS message to the persons in RECORDS with SUBJECT."
  (interactive (list (if (bbdb-do-all-records-p)
                         (mapcar 'car bbdb-records)
                       (list (bbdb-current-record)))))
  (let ((good '()) (bad '())
        (orec records)
        m)
    ;; collect the mobile numbers
    (while records
      (if (setq m (bbdb-make-sms-address (car records)))
          (setq good (cons m good))
        (setq bad (cons (car records) bad)))
      (setq records (cdr records)))
    (bbdb-send-mail-internal
     (mapconcat 'identity (nreverse good) ",\n    ")
     subject orec)
    ;; cleanup of body
    (mail-text)
    (delete-region (point) (point-max))
    ;; insert wrong records names
    (if (not bad) nil
      (goto-char (point-max))
      (let ((p (point))
            (fill-prefix "    ")
            (fill-column 70))
        (insert "*** Warning: No mobile phone for "
                (mapconcat (lambda (x) (bbdb-record-name x))
                           (nreverse bad) ", ") ".")
        (fill-region-as-paragraph p (point))
        (goto-char p))))
  (let ((headers bbdb/sms-headers))
    (while headers
      (mail-position-on-field (caar headers))
      (let ((end (point)))
        (re-search-backward (concat "^" (caar headers) ":"))
        (delete-region (point) end))
      (cond ((null (cadar headers))
             (delete-char 1))
            ((stringp (cadar headers))
             (insert (caar headers) ": " (cadar headers)))
            ((functionp (cadar headers))
              (insert (caar headers) ": " (funcall (cadar headers))))
            (t
             (insert (caar headers) ": " (eval (cadar headers)))))
      (setq headers (cdr headers))))
  ;; install a consumed characters feedback
  (mail-text)
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 'bbdb-sms-max-chars))

(defun bbdb-sms-max-chars (&optional b e txt)
  "Print a message about the current number of characters in the SMS.

This function is used as `after-change-functions` and thus takes B, E and TXT."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote mail-header-separator) (point-max) t)
        (let ((chars (- (point-max) (point))))
          ;; if there is an subject let it contribute to the chars
          (if (re-search-backward "^Subject:" (point-min) t)
              (let ((start (point)))
                (goto-char (match-end 0))
                (re-search-forward "^[^ \t]")
                (setq chars (+ chars (- (point) start 8)))))
          ;; show feedback!
          (when (> chars 0)
            (message "%d chars left! Your SMS has now %d chars! %d is maximum!"
                     (- bbdb/sms-max-chars chars) chars bbdb/sms-max-chars)
            ;; and ding if exceeded
            (if (> chars bbdb/sms-max-chars)
                (ding)))))))

;;;###autoload
(defun bbdb-sms-yank-mobiles ()
  "Add people displayed in the *BBDB* buffer on this SMS recipients list."
  (interactive)
  (let ((addrs (save-excursion
                 (set-buffer bbdb-buffer-name)
                 (delq nil
                       (mapcar (lambda (x) (bbdb-make-sms-address (car x)))
                               bbdb-records)))))
    (goto-char (point-min))
    ;; If there's a CC field, move to the end of it, inserting a comma if
    ;;  there are already addresses present.
    ;; Otherwise, if there's an empty To: field, move to the end of it.
    ;; Otherwise, insert an empty CC: field.
    (re-search-forward "^To:[ \t]*")
    (if (eolp)
        nil
      (end-of-line)
      (while (looking-at "\n[ \t]")
        (forward-char) (end-of-line))
      (insert ",\n")
      (indent-relative))
    (if (eolp)
        nil
      (end-of-line)
      (while (looking-at "\n[ \t]")
        (forward-char) (end-of-line))
      (insert "\nCC:")
      (indent-relative))
    ;; Now insert each of the addresses on its own line.
    (while addrs
      (insert (car addrs))
      (if (cdr addrs) (progn (insert ",\n") (indent-relative)))
      (setq addrs (cdr addrs)))))


;;;###autoload
(defcustom rf-bbdb/vm-primary-inbox-regexp
  (if (boundp 'vm-primary-inbox) vm-primary-inbox)
  "A regexp matching folder names of primary VM inboxes."
  :group 'bbdb-rf
  :type 'string)

;;;###autoload
(defun rf-bbdb/vm-ignore-old-folders ()
  "Hook for ignoring all folders except in-boxes.

Set `bbdb/mail-auto-create-p' to this function in order to ignore new
addresses in all folders except the `vm-primary-inbox' or those matching
`bbdb/vm-primary-inbox-regexp'."
  (interactive)
  (save-excursion
    (vm-select-folder-buffer)
    (if (and (string-match (or rf-bbdb/vm-primary-inbox-regexp
                               vm-primary-inbox)
                           (buffer-name))
             (bbdb-ignore-some-messages-hook))
        'prompt)))

;;;###autoload
(defun rf-bbdb/vm-ignore-old-addresses ()
  "Hook for ignoring all addresses except in in-boxes.

Set `bbdb-always-add-addresses' to this function in order to ignore new
addresses in all folders except the `vm-primary-inbox' or those matching
`bbdb/vm-primary-inbox-regexp'."
  (if (member major-mode '(vm-mode vm-virtual-mode vm-summary-mode
                                   vm-presentation-mode))
      (save-excursion
        (vm-select-folder-buffer)
        (if (string-match (or rf-bbdb/vm-primary-inbox-regexp
                              vm-primary-inbox)
                          (buffer-name))
            'ask                         ; ask the user
          nil                        ; do not add it
          ))
    nil))

;;;###autoload
(defun bbdb-clean-nets (records)
  "Remove dublicate nets from all visible RECORDS."
  (interactive (list (mapcar 'car bbdb-records)))
  (let (nets cnets)
    (while records
      (setq nets (bbdb-record-net (car records))
            cnets nil)
      (while nets
        (add-to-list 'cnets (car nets))
        (setq nets (cdr nets)))
      (bbdb-record-set-net (car records) cnets)
      (setq records (cdr records)))))

;;;###autoload
(defun bbdb/vm-grab-homepage ()
  "Grab the current URL and store it in the record correspondig to the sender."
  (interactive)
  ;; if there is no database record for this person, create one
  (let ((record (save-excursion (or (bbdb/vm-update-record t) 
				    (error "Failed to update the record."))))
        (url (save-excursion
               (re-search-backward "\\s-" (point-min) t)
               (if (re-search-forward vm-url-regexp (point-max) t)
                   (match-string 2)))))
    (if (bbdb-record-getprop record 'www)
        (bbdb-record-putprop
         record 'www
         (concat (bbdb-record-getprop record 'www) ", " url))
      (bbdb-record-putprop record 'www url))
    (bbdb-change-record record t)
    (bbdb-display-records (list record))))

;;;###autoload
(defcustom bbdb-snarf-format
  '(("default"
     (name    1 "^.*$")
     (phones  3 "^\\(\\w+:\\)?\\s *[+()0-9][0-9() \t/-]+[0-9]+$")
     (nets    3 "[^ \t\n<]+@[^ \t\n>]+")
     (www     1 "\\(http://\\|www\.\\)[^ \t\n]+")
     (street  1 "^\\w+.*[0-9/-]+\\w*.*")
     (zip     1 "\\([A-Z]-\\)?[0-9][0-9][0-9][0-9]+")
     (city    1 "\\w+[^ ,\n]*")
     (state   1 "\\w+")
     (country 1 "\\w+")))
  "*An alist of snarfing strategies for `bbdb-snarf-region-better'.

The keys of the alist are the strategy names.

A strategy is a list of the following elements:

    (BBDB-FIELD-NAME COUNT REGEXP)

During snarfing the elements of the strategy will be processed in sequential
order.  REGEXP will be searched for COUNT times and if a match occurs it will
be appended to the bbdb field BBDB-FIELD-NAME and is removed from the snarfing
buffer.  The unmatched rest goes to the notes."
  :group 'bbdb-rf
  :type 'sexp)

(defcustom bbdb-csv-export-fields
  '(
    (web.de
     ("nickname" mail-alias)
     ("Vorname" firstname)
     ("Nachname" lastname)
     ("Straﬂe privat" street "home")
     ("Postleitzahl privat" zip "home")
     ("Ort privat" city "home")
     ("Staat privat" country "home")
     ("Geburtstag" birthday)
     ("Notizen" notes)
     ("Web-Seite" www)
     ("E-Mail-Adresse" email)
     ("E-Mail 2: Adresse" email 1)
     ("Telefon privat" phone "home")
     ("Telefon gesch‰ftlich" phone "job")
     ("Mobilfunktelefon" phone "mobile")
     ("Fax privat" phone "home fax"))
    (gmx.de
     ("nickname" mail-alias)
     ("Vorname" firstname)
     ("Nachname" lastname)
     ("Straﬂe privat" street "home")
     ("Postleitzahl privat" zip  "home")
     ("Ort privat" city  "home")
     ("Staat privat" country  "home")
     ("Geburtstag" birthday)
     ("Notizen" notes)
     ("Web-Seite" www)
     ("E-Mail-Adresse" email)
     ("e-mail-2-Adresse" email 1)
     ("Telefon privat" phone  "home")
     ("Telefon gesch‰ftlich" phone  "job")
     ("Mobilfunktelefon" phone "mobile")
     ("Fax privat" phone "home fax")
     )
    (outlook-german
     ("Vorname" firstname)
     ("Nachname" lastname)
     ("Geburtstag" birthday)
     ("Kategorien" dir)
     ("E-Mail-Adresse" email)
     ("E-Mail 2: Adresse" email 1)
     ("Webseite" www)
     ("Straﬂe privat" street "home")
     ("Postleitzahl privat" zip  "home")
     ("Ort privat" city  "home")
     ("Land privat" country  "home")
     ("Telefon privat" phone  "home")
     ("Fax privat" phone "home fax")
     ("Mobiltelefon" phone "mobile")
     ("Telefon gesch‰ftlich" phone  "job")
     ("Notizen" notes))
    (outlook
     ("Title")
     ("First Name" firstname)
     ("Middle Name")
     ("Last Name" lastname)
     ("Suffix")
     ("Company" company)
     ("Department")
     ("Job Title")
     ("Business Street" street "Office")
     ("Business Street 2")
     ("Business Street 3")
     ("Business City" city  "Office")
     ("Business State" state  "Office")
     ("Business Postal Code" zip  "Office")
     ("Business Country" country  "Office")
     ("Home Street" street "Home")
     ("Home Street 2")
     ("Home Street 3")
     ("Home City" city  "Home")
     ("Home State" state  "Home")
     ("Home Postal Code" zip  "Home")
     ("Home Country" country  "Home")
     ("Other Street" street (not "Home\\|Office"))
     ("Other Street 2")
     ("Other Street 3")
     ("Other City" city (not "Home\\|Office"))
     ("Other State" state (not "Home\\|Office"))
     ("Other Postal Code" zip (not "Home\\|Office"))
     ("Other Country" country (not "Home\\|Office"))
     ("Assistant's Phone")
     ("Business Fax" phone "Fax")
     ("Business Phone" phone  "Office")
     ("Business Phone 2" phone  "Office" 1)
     ("Business Phone 3" phone  "Office" 2)
     ("Callback")
     ("Car Phone")
     ("Company Main Phone")
     ("Home Fax")
     ("Home Phone" phone  "Home")
     ("Home Phone 2" phone  "Home" 1)
     ("ISDN")
     ("Mobile Phone" phone "Cell")
     ("Mobile Phone 2" phone "Cell" 1)
     ("Mobile Phone 3" phone "Cell" 2)
     ("Other Fax")
     ("Other Phone" phone (not "Fax\\|Home\\|Cell\\|Office"))
     ("Other Phone 2" phone (not "Fax\\|Home\\|Cell\\|Office") 1)
     ("Other Phone 3" phone (not "Fax\\|Home\\|Cell\\|Office") 2)
     ("Pager")
     ("Primary Phone" phone ("home" "mobile" "job"))
     ("Radio Phone")
     ("TTY/TDD Phone")
     ("Telex")
     ("Account")
     ("Anniversary")
     ("Assistant's Name")
     ("Billing Information")
     ("Birthday")
     ("Categories" mail-alias)
     ("Categories 2" mail-alias 1)
     ("Categories 3" mail-alias 2)
     ("Categories 4" mail-alias 3)
     ("Children")
     ("Directory Server")
     ("E-mail Address" email)
     ("E-mail Display Name")
     ("E-mail 2 Address" email 1)
     ("E-mail 2 Display Name")
     ("E-mail 3 Address" email 2)
     ("E-mail 3 Display Name")
     ("Gender")
     ("Government ID Number")
     ("Hobby")
     ("Initials")
     ("Internet Free Busy")
     ("Keywords")
     ("Language")
     ("Location")
     ("Manager's Name")
     ("Mileage")
     ("Notes" notes)
     ("Office Location")
     ("Organizational ID Number")
     ("PO Box")
     ("Priority")
     ("Private")
     ("Profession")
     ("Referred By" aka)
     ("Sensitivity")
     ("Spouse")
     ("User 1")
     ("User 2")
     ("User 3")
     ("User 4")
     ("Web Page" www))
    (kde3.1-german
     ("Formatierter Name")
     ("Nachname" lastname)
     ("Vorname" firstname)
     ("Zus‰tzliche Namen")
     ("Titel-Abk. (vorn)")
     ("Titel-Abk. (hinten)")
     ("Spitzname" aka)
     ("Geburtstag" birthday)
     ("Straﬂe (Privatadresse)" street "home")
     ("Ort (Privatadresse)" city  "home")
     ("Region (Privatadresse)" state "home")
     ("Postleitzahl (Privatadresse)" zip "home")
     ("Land (Privatadresse)" country  "home")
     ("Aufkleber (Privatadresse)")
     ("Straﬂe (Firmenadresse)" street "job")
     ("Ort (Gesch‰ftsadresse)" city "job")
     ("Region (Gesch‰ftsadresse)" state "job")
     ("Postleitzahl (Firmenadresse)" zip "job")
     ("Land (Firmenadresse)" country "job")
     ("Aufkleber (Gesch‰ftsadresse)")
     ("Privattelefon" phone "home")
     ("Telefon (gesch‰ftlich)" phone "job")
     ("Mobiltelefon" phone "mobile")
     ("Fax (privat)" phone "home fax")
     ("Fax (gesch‰ftlich)" phone "job fax")
     ("Autotelefon")
     ("ISDN")
     ("Pager")
     ("Mail-Adresse" email)
     ("Mail-Programm")
     ("Titel")
     ("Rolle")
     ("Organisation/Firma" company)
     ("Notiz" notes)
     ("Adresse")
     ("Abteilung")
     ("Beruf")
     ("Name des Assistenten")
     ("Name des Managers")
     ("Name des Ehegatten")
     ("B¸ro")
     ("IM-Adresse")
     ("Jubil‰um")
     )
    (thunderbird
     ("First Name" firstname)
     ("Last Name" lastname)
     ("Display Name" name)
     ("Nickname" aka)
     ("Primary Email" email)
     ("Secondary Email" email 1)
     ("Work Phone" phone "work" "office")
     ("Home Phone" phone "home")
     ("Fax Number" phone "fax")
     ("Pager Number" phone "pager")
     ("Mobile Number" phone ("mobile" "cell"))
     ("Home Address" street "home")
     ("Home Address 2")
     ("Home City" city "home")
     ("Home State" state "home")
     ("Home ZipCode" zip "home")
     ("Home Country" zip "country")
     ("Work Address" street ("work" "office"))
     ("Work Address 2")
     ("Work City" city ("work" "office"))
     ("Work State" state ("work" "office"))
     ("Work ZipCode" zip ("work" "office"))
     ("Work Country" country ("work" "office"))
     ("Job Title")
     ("Department")
     ("Organization" company)
     ("Web Page 1" www)
     ("Web Page 2")
     ("Birth Year")
     ("Birth Month")
     ("Birth Day")
     ("Custom 1" mail-alias)
     ("Custom 2")
     ("Custom 3")
     ("Custom 4")
     ("Notes" notes)
     )
    )
  "*An alist of CSV exports and their field names and associated BBDB fields.

The format of an element in each sublist is one of the following:

For any BBDB field
    (CSV-HEADER BBDB-FIELD)
For email, mail-alias:
    (CSV-HEADER BBDB-FIELD POSITION)
For street, zip, city, state, county:
    (CSV-HEADER BBDB-FIELD BBDB-FIELD-LABEL)
    (CSV-HEADER BBDB-FIELD BBDB-FIELD-LABEL POSITION)

CSV-HEADER is a string as obtained from an CSV-export.
BBDB-FIELD is the internal symbol of the designated BBDB-field
POSITION is an position, counting starts with 0!
BBDB-FIELD-LABEL is a regexp, or (not regexp) or a list of rexexps.

See the defaults for an starting point.
I am to lazy to write a custom-spec for this, but if you provide one to me I
will luckily  add it! ;c)"
  :group 'bbdb-rf
  :type 'sexp)

(defcustom bbdb-csv-export-file "~/bbdb.csv"
  "*Filename exports will be written to."
  :group 'bbdb-rf
  :type 'filename)

(defcustom bbdb-csv-export-type 'outlook-german
  "*The type of export.
See `bbdb-csv-export-fields' for valid types!"
  :group 'bbdb-rf
  :type (cons 'choice
              (mapcar (lambda (x) (list 'const (car x)))
                      bbdb-csv-export-fields)))

(defcustom bbdb-csv-export-newline "\r\n"
  "*The the line ending character."
  :group 'bbdb-rf
  :type 'string)

(defvar bbdb-csv-export-type-history nil)

;;;###autoload
(defun bbdb-csv-export (&optional file export-type)
  "Export all currently displayed records to FILE honoring EXPORT-TYPE.
The records are written in CVS-format (comma separated values) which can be
imported by nearly all clients, i.e. Outlook, Netscape, Mozilla, KAddress,
... each client expects a specific layout which can be set by export-type.

See `bbdb-csv-export-fields` for predefined export-types."
  (interactive
   (if current-prefix-arg
       (list
        (read-file-name "BBDB CSV export file: "
                        (file-name-directory bbdb-csv-export-file)
                        bbdb-csv-export-file
                        nil
                        (file-name-nondirectory bbdb-csv-export-file))
        (intern
         (completing-read (format "BBDB CVS export type%s: "
                                  (if bbdb-csv-export-type-history
                                      (format
                                       " [%s]"
                                       (car bbdb-csv-export-type-history))
                                    ""))
                          (mapcar (lambda (x) (list (format "%s" (car x))))
                                  bbdb-csv-export-fields)
                          nil t nil
                          'bbdb-csv-export-type-history
                          (car bbdb-csv-export-type-history))))))
         
  (setq file (or file bbdb-csv-export-file)
        bbdb-csv-export-file file
        export-type (or export-type bbdb-csv-export-type)
        bbdb-csv-export-type export-type)

  (let ((records (save-excursion (set-buffer bbdb-buffer-name) bbdb-records))
        (fields (cdr (assoc export-type bbdb-csv-export-fields)))
        (props (mapcar '(lambda (p) (intern (car p))) (bbdb-propnames)))
        rec rawnotes)

    ;; open and erase output file
    (set-buffer (find-file-noselect (or file bbdb-csv-export-file)))
;;    (setq buffer-file-coding-system 'raw-text)
    (erase-buffer)
    
    ;; create headers
    (let ((fields fields))
      (while fields
        (insert (format "%S" (caar fields)) (if (cdr fields) "," ""))
        (setq fields (cdr fields)))
      (insert bbdb-csv-export-newline))

    ;; now insert all the currently visible records
    (while records
      (setq rec (caar records)
            rawnotes (bbdb-record-raw-notes rec)
            records (cdr records))
      (let ((fields fields)
            ffun ;; field function
            fh ;; field heading as in CSV file
            fn ;; field name in BBDB
            fl ;; field label in BBDB
            flc ;; field label check
            fle ;; field label regexp-list
            fp ;; field position in BBDB for fields with the same label
            val ;; value of the field
            )
        (while fields
          (setq fh (car fields)
                fn (cadr fh)
                fp (or (cadddr fh) 0)
                fl (caddr fh)
                ;; `val' will contain the actual label-value
                flc '(string-match (car fle) val)
                fle (cond ((not fl) nil)
                          ((listp fl)
                           (cond ((stringp (car fl))
                                  fl)
                                 ((eq (car fl) 'not)
                                  (setq flc '(not (string-match (car fle) val)))
                                  (cdr fl))
                                 (t
                                  (error "Unknown BBDB-FIELD-LABEL format `%s' fl"))))
                          ((stringp fl)
                           (list fl))
                          ((numberp fl) ;; so it actually was an position?
                           (setq fp fl fl nil)
                           nil)
                          (t
                           (error "Invalid field type <%S>!" fl)))
                fh (car fh)
                val (cond
                     ;; address
                     ((member fn '(address street zip city state country))
                      (let (addr)
                        (while (and fle (not addr))
                          (setq addr (mapcar
                                      (lambda (p)
                                        (setq val (aref p 0))
                                        (if (eval flc) p))
                                      (bbdb-record-addresses rec))
                                addr (delete nil addr)
                                fle (cdr fle)))
                        (when addr
                          (setq addr (nth fp addr))
                          (cond
                           ((eq fn 'street)
                            (if (and addr (setq val
                                                (bbdb-address-streets addr))
                                     val (setq val
                                               (mapconcat 'identity val ", ")))
                                val))
                           (t
                            (eval (list
                                   (intern (format "bbdb-address-%s"
                                                   fn)) addr)))))))
                     ;; phones
                     ((eq fn 'phone)
                      (let (phone)
                        (while (and fle (not phone))
                          (setq phone (mapcar
                                       (lambda (p)
                                         (setq val (aref p 0))
                                         (if (eval flc) (aref p 1)))
                                       (bbdb-record-phones rec))
                                phone (delete nil phone)
                                fle (cdr fle)))
                        (if phone (nth fp phone))))
                     ;; email
                     ((eq fn 'email)
                      (let ((nets (bbdb-record-net rec)))
                        (if nets
                            (if fp
                                (and (< fp (length nets)) (nth fp nets))
                              (car nets)))))
                     ;; mail-alias
                     ((eq fn 'mail-alias)
                      (let ((aliases (assoc 'mail-alias rawnotes)))
                        (when aliases
                          (setq aliases (bbdb-split (cdr aliases) ","))
                          (if fp
                              (and (< fp (length aliases)) (nth fp aliases))
                            (car aliases)))))
                     ;; timestamp
                     ((eq fn 'timestamp)
                      (let ((timestamp (assoc 'timestamp rawnotes)))
                        (when timestamp
                          (setq timestamp (bbdb-split (cdr timestamp) ","))
                          (if fp
                              (and (< fp (length timestamp)) (nth fp timestamp))
                            (car timestamp)))))
                     ;; AKA
                     ((eq fn 'aka)
                      (setq val (car (bbdb-record-aka rec))))
                     ;; any other user-defined properties
                     ((member fn props)
                      (setq val (assoc fn rawnotes))
                      (cdr val))
                     ((condition-case nil
                        (progn
                          (setq ffun (intern (format "bbdb-record-%s" fn)))
                          (symbol-function ffun))
                        (error nil))
                      (setq val (eval (list ffun rec)))
                      val)
                     ;; default rule
                     ((eq fn nil)
                      ;; do nothing
                      nil)
                     (t
                      (error "Field type '%s'is undefined for your BBDB!" fn)
                      nil)))

          (when (and val (string-match "\\S-" val))
            (if (string-match "\"" val)
                (setq val (bbdb-replace-in-string val "\"" "\"\"")))
            (insert "\"" val "\""))
          
          ;; insert comma if necessary, i.e. not last field
          (if (cdr fields) (insert ","))
          (setq fields (cdr fields))))
      (insert bbdb-csv-export-newline)))
  (save-buffer))


(defun bbdb-print-latex-format-address (a)
  "Formats address A for latex precessing."
  (let* ((street  (mapconcat (lambda (l) (bbdb-print-tex-quote l))
                             (nth 1 a)  "\\\\"))
         (city    (bbdb-print-tex-quote (nth 2 a)))
         (state   (bbdb-print-tex-quote (nth 3 a)))
         (zip     (bbdb-print-tex-quote (nth 4 a)))
         (country (bbdb-print-tex-quote (nth 5 a))))
    
    (setq street  (if (string= "" street)
                      (bbdb-print-tex-quote "___________ __")
                    street)
          state   (if (not (string= "" state)) (concat ", " state))
          country (if (not (string= "" country)) (concat "\\\\" country)))
    
    (if (and bbdb-continental-zip-regexp
             (string-match bbdb-continental-zip-regexp zip))
        (concat street "\\\\"
                zip " " city state
                country)
      (concat street "\\\\"
              city state " " zip
              country))))

(defun bbdb-print-latex-format-phone (phone)
  "Formats PHONE for latex precessing."
  (let ((start 0))
    (while (setq start (string-match "\\([^0-9+]\\)" phone start))
      (setq phone (replace-match "\\1\"\"" nil nil phone)
            start (+ 4 start)))
    (setq start 0)
    (while (setq start (string-match
                        "\\([0-9][0-9][0-9]\\)\\([0-9]+\\)"
                        phone start))
      (if (or (< 1 (length (match-string 2 phone))))
          (setq phone (replace-match "\\1\\\\,\\2" nil nil phone)
                start (+ 5 start))
        (setq start (1+ start))))
    (setq start 0)
    (while (setq start (string-match
                        "\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
                        phone start))
      (setq phone (replace-match "\\1\\\\,\\2" nil nil phone)
            start (+ 3 start)))
    phone))

(defcustom bbdb-print-latex-list-table
  '(("[p]" . "private")
   ("[c]" . "company")
   ("." . "All Addresses)"))
  "*A alist mapping predicates to labels."
  :group 'bbdb-rf
  :type '(repeat (cons (regexp :tag "Regexp") (string :tag "Label"))))
  
(defcustom bbdb-print-latex-list-predicate ".*"
  "*The default predicate."
  :group 'bbdb-rf
  :type '(regexp :tag "Regexp"))

(defcustom bbdb-print-latex-list-field 'dir
  "*You BBDB field for making categories etc."
  :group 'bbdb-rf
  :type (cons 'choice (mapcar (lambda (f) (list 'const (intern (car f))))
                              (bbdb-propnames))))

(defvar bbdb-print-latex-list-history nil)

;;;###autoload
(defun bbdb-print-latex-list (predicate)
  "List all BBDB records matching PREDICATE.

Predicate is a regexp matching the value of `bbdb-print-latex-list-field`."
  (interactive (list (completing-read "Predicate: "
                                      bbdb-print-latex-list-table
                                      nil nil
                                      bbdb-print-latex-list-predicate
                                      bbdb-print-latex-list-history)))
  
  (setq bbdb-print-latex-list-predicate predicate)
  
  (let ((records (bbdb-records))
        rec notes display)
    
    (while records
      (setq rec (car records) records (cdr records))
      (setq notes (bbdb-record-raw-notes rec))
      (if (and (assoc bbdb-print-latex-list-field notes)
               (string-match predicate
                             (cdr (assoc bbdb-print-latex-list-field notes))))
          (setq display (cons rec display))))

    (setq display (sort
                   display
                   (lambda (r1 r2)
                     (or (string< (bbdb-record-lastname r1)
                                  (bbdb-record-lastname r2))
                         (and (string= (bbdb-record-lastname r1)
                                       (bbdb-record-lastname r2))
                              (string< (bbdb-record-firstname r1)
                                       (bbdb-record-firstname r2)))))))

    (bbdb-display-records display)))

;;;###autoload
(defun bbdb-print-latex (to-file)
  "Writes currently visible records in LaTeX format TO-FILE.

Make a LaTeX file for printing out the bbdb database.\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-print]\" is \
used instead of simply \"\\[bbdb-print]\", then includes only the
people currently in the *BBDB* buffer.

There are various variables for customizing the content & format of
the printout, notably the variables `bbdb-print-alist' and
`bbdb-print-require'.  See the file bbdb-print.el for more information.

And probably you also want the TeX source for this!
They are on the same site as this file!"
  (interactive (list
                (if current-prefix-arg
                    (read-file-name "Print To File: "
                                    (file-name-directory bbdb-print-file-name)
                                    bbdb-print-file-name
                                    nil
                                    (file-name-nondirectory
                                     bbdb-print-file-name))
                  bbdb-print-file-name)))
  
  (setq bbdb-print-file-name (expand-file-name to-file))

  (let* ((records (save-excursion
                    (set-buffer bbdb-buffer-name)
                    (mapcar 'car bbdb-records)))
         (count (length records)) (current 0)
         current-letter
         rec name company nets
         phones addresses locations p a l
         notes)
    
    (set-buffer (find-file-noselect bbdb-print-file-name))
    (widen)
    (erase-buffer)

    (if (assoc bbdb-print-latex-list-predicate bbdb-print-latex-list-table)
        (insert "\\bbdbtitle{"
                (cadr (assoc bbdb-print-latex-list-predicate
                             bbdb-print-latex-list-table))
                "}{" bbdb-print-latex-list-predicate "}\n\n")
      (insert "\\bbdbtitle{Selection=}{"
              bbdb-print-latex-list-predicate "}\n\n"))
    
    (while records
      (when (= 0 (% current 10))
        (message "%d%% processed" (/ (* current 100) count))
        (sit-for 0))
      (setq current (1+ current)
            rec (car records)
            company (bbdb-record-company rec)
            name (bbdb-record-name rec)
            nets (bbdb-record-net rec)
            phones (mapcar (lambda (p) (append p nil))
                           (bbdb-record-phones rec))
            addresses (mapcar (lambda (p) (append p nil))
                              (bbdb-record-addresses rec))
            notes (bbdb-record-raw-notes rec))
      
      ;; collect the locations
      (let (l p (plocs phones))
        (while plocs
          (setq p (car plocs) l (car p))
          (if (all-completions l addresses)
              (setq locations (cons (list l (cadr p)
                                          (assoc l addresses))
                                    locations)
                    phones (delq p phones)
                    addresses (delq (assoc (car p) addresses) addresses)))
          (setq plocs (cdr plocs))))

      (setq locations
            (sort
             (append (mapcar (lambda (i) (cons 'location i)) locations)
                     (mapcar (lambda (i) (cons 'phone i)) phones)
                     (mapcar (lambda (i) (cons 'address i)) addresses))
             (lambda (i1 i2)
               (or (string-match (concat "^" (cadr i1)) (cadr i2))
                   (string-match (concat "^" (cadr i2)) (cadr i1))
                   (string< (cadr i1) (cadr i2))))))
      
      ;; letter
      (when (and (bbdb-record-lastname rec)
                 (not (eq current-letter
                          (aref (bbdb-record-lastname rec) 0))))
        (setq current-letter (aref (bbdb-record-lastname rec) 0))
        (insert (format "\\bbdbletter{%c}\n" current-letter)))
        
        
      ;; record
      (insert "\\begin{bbdbrecord}\n")
      (if name (insert "\t\\bbdbname{"
                       (if (assoc bbdb-print-latex-list-field notes)
                           (bbdb-print-tex-quote
                            (cdr (assoc bbdb-print-latex-list-field notes)))
                         "")
                       "}{"
                       (bbdb-print-tex-quote name)
                       "}{"))
      (if (and notes (assoc 'birthday notes))
          (insert (bbdb-print-tex-quote (cdr (assoc 'birthday notes)))))
      (insert "}{")
      (if company
          (insert (bbdb-print-tex-quote company)))
      (insert "}\n")

      (while locations
        (setq l (car locations) locations (cdr locations))
        (cond ((eq (car l) 'phone)
               (setq p (cdr l))
               (insert "\t\\bbdbphone{"
                       (bbdb-print-tex-quote (car p))
                       "}{"
                       (bbdb-print-tex-quote
                        (bbdb-print-latex-format-phone (cadr p)))
                       "}\n"))
              ((eq (car l) 'address)
               (setq a (cdr l))
               (insert  "\t\\bbdbaddress{" (bbdb-print-tex-quote (car a))
                        "}{"
                        (bbdb-print-latex-format-address a)
                        "}\n"))
              ((eq (car l) 'location)
               (setq l (cdr l))
               (insert  " \t\\bbdblocation{"
                        (bbdb-print-tex-quote (car l))
                        "}{"
                        (bbdb-print-tex-quote
                         (bbdb-print-latex-format-phone (cadr l)))
                        "}{"
                        (bbdb-print-latex-format-address (caddr l))
                        "}\n"))))

      (when nets
        (if (> (length nets) 2)
            (setq nets (list (car nets) (cadr nets))))
        (insert "\t\\bbdbnets{"
                (mapconcat (lambda (n) (bbdb-print-tex-quote n))
                           nets
                           "\\\\")"}\n"))
        
      (if (and notes (setq notes (assoc 'notes notes)))
          (insert  " \t\\bbdbnotes{" (bbdb-print-tex-quote
                                      (cdr notes)) "}\n"))
        
      (insert "\\end{bbdbrecord}\n")
        
      (setq records (cdr records)))
      
    (goto-char (point-min))
    (save-buffer)))

;;;###autoload
(defun bbdb-snarf-region-better (begin end)
  "Snarf up a BBDB record in the region from BEGIN to END.
I tried a better snarfing here.  See `bbdb-snarf' for the original.

When called with a prefix arg, prompt for a strategy."
  (interactive "r")

  (save-excursion
    (let ((buf (get-buffer-create " *BBDB snarf*"))
          (text (buffer-substring-no-properties begin end))
          name company nets www phones street zip city state country notes
          label phones-vector address-vector val)
      
      (set-buffer buf)
      (erase-buffer)
      (insert text)

      ;; toss beginning, trailing and bogus space
      (goto-char (point-min))
      (while (re-search-forward "^\\s-+" (point-max) t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\\s-+$" (point-max) t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "[\t ]+" (point-max) t)
        (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "[,;:.]$" (point-max) t)
        (replace-match ""))
      (goto-char (point-min))
      
      (let ((fmt (cdr (assoc (if current-prefix-arg
                                 (completing-read "Snarfing mode: "
                                                  (mapcar (lambda (mode)
                                                            (list (car mode)))
                                                          bbdb-snarf-format)
                                                  nil t nil nil
                                                  "default")
                               "default")
                             bbdb-snarf-format)))
            sym cnt re)
        (while fmt
          (setq sym (caar fmt)
                cnt (cadar fmt)
                re  (caddar fmt)
                fmt (cdr fmt))
          (goto-char (point-min))
          (while (> cnt 0)
            (setq cnt (1- cnt))
            (if (not (re-search-forward re (point-max) t))
                (setq cnt 0)
              (setq val (symbol-value sym)
                    val (append val (list (match-string 0))))
              (set sym val)
              (replace-match "")))))

      ;; The rest goes into the notes
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" (point-max) t)
        (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "\\s-+\\|[,./:;-]+" (point-max) t)
        (replace-match ""))
      (bbdb-snarf-prune-empty-lines)
      (if (/= (point-min) (point-max))
          (setq notes (append notes (list (cons 'notes (buffer-string))))))

      (if www
          (setq notes
                (append notes (list (cons bbdb-snarf-web-prop www)))))
      
      ;; create phones vector
      (while phones
        (erase-buffer)
        (insert (car phones))
        (goto-char (point-min))
        (if (re-search-forward "[()+0-9]" (point-max) t)
            (goto-char (match-beginning 0)))
        (setq label (bbdb-snarf-extract-label
                     (bbdb-label-completion-default 'phone)
                     t))
        (setq phones-vector
              (append phones-vector
                      (list (vconcat (list label)
                                     (bbdb-snarf-parse-phone-number
                                      (buffer-substring
				       (point-min) (point-max))))))
              phones (cdr phones)))
      
      ;; create address vector
      (setq address-vector (list (bbdb-snarf-make-address
                                  (bbdb-label-completion-default
                                   'address)
                                  street
                                  (or (car city) "")
                                  (or (car state) "")
                                  (or (car zip) "")
                                  (or (car country) ""))))
      
      ;; and finnaly merge into BBDB
      (bbdb-merge-interactively (car name)
                                (car company)
                                nets
                                address-vector
                                phones-vector
                                notes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom bbdb-vcal-export-peek 9
  "Number of months from now on to peek ahead for birthdays.
Birthdays not within the range will be neglected!"
  :group 'bbdb-rf
  :type 'integer)

(defcustom bbdb-vcal-export-dir "~/s45i/vcal"
  "VCAL entries will be exported to this directory."
  :group 'bbdb-rf
  :type 'directory)

(defun bbdb-birthday-vcal-export ()
  "Export birthday of currently displayed records to VCS files.

This function is intended to be used with the command line tool `scmxx'
for accessing Siemens mobile phones.

cd ~/s45i/vcal
# delete existing calendar??? Really?
scmxx -r -Call
# send new slots
scmxx -s -Call bbdb*"
  (interactive)
  (let ((records (bbdb-records))
        rec notes birthday time
        year month day type (num 0)
        (current-month (nth 4 (decode-time)))
        (current-year (nth 5 (decode-time))))
    (while records
      (setq rec (car records)
            records (cdr records)
            notes (bbdb-record-raw-notes rec))
      (when (setq birthday (assoc 'birthday notes))
        (setq birthday (cdr birthday))
        (while (string-match
                "\\([*+] \\)?\\([0-9]?[0-9]\\.[0-9]?[0-9]\\.\\([0-9][0-9][0-9][0-9]\\)?\\)"
                birthday)
          (setq time (bbdb-split (match-string 2 birthday) ".")
                year (string-to-number (or (nth 2 time) "1900"))
                month (string-to-number (nth 1 time))
                day (string-to-number (nth 0 time))
                type (or (match-string 1 birthday) "*"))
          (setq birthday (substring birthday (match-end 0)))
          (if (<= bbdb-vcal-export-peek (if (<= current-month month)
                                           (- month current-month)
                                         (+ (- 12 current-month) month)))
              (message "%s: %s %s ignored" (bbdb-record-name rec) type time)
            (message "%s: %s %s created" (bbdb-record-name rec) type time)
            (find-file (expand-file-name (format "bbdb%02d.vcs" num)
                                         bbdb-vcal-export-dir))
            (setq num (1+ num))
            (erase-buffer)
            (setq year (if (<= current-month month) current-year
                              (1+ current-year)))
            (insert (format "BEGIN:VCALENDAR
VERSION:1.0
BEGIN:VEVENT
CATEGORIES:ANNIVERSARY
DALARM:%04d%02d%02dT100000
DTSTART:%04d%02d%02dT000000
DTEND:%04d%02d%02dT235959
RRULE:YD1
DESCRIPTION:%s %s
END:VEVENT
END:VCALENDAR
"
                            year month day
                            year month day
                            year month day
                            type (bbdb-record-name rec)))
            (save-buffer)
            (kill-buffer (current-buffer))
            ))))))


(defun bbdb-birthday-diary-export ()
  "Export birthday of currently displayed records to diary."
  (interactive)
  (let ((records (bbdb-records))
        rec notes birthday time
        year month day type)
    (find-file diary-file)
    (goto-char (point-min))
    (delete-matching-lines "^%%(diary-anniversary[^)]+) [*+]")
    (goto-char (point-min))
    (while records
      (setq rec (car records)
            records (cdr records)
            notes (bbdb-record-raw-notes rec))
      (when (setq birthday (assoc 'birthday notes))
        (setq birthday (cdr birthday))
        (while (string-match
                "\\([*+] \\)?\\([0-9]?[0-9]\\.[0-9]?[0-9]\\.\\([0-9][0-9][0-9][0-9]\\)?\\)"
                birthday)
          (setq time (bbdb-split (match-string 2 birthday) ".")
                year (string-to-number (or (nth 2 time) "1900"))
                month (string-to-number (nth 1 time))
                day (string-to-number (nth 0 time))
                type (or (match-string 1 birthday) "*"))
          (setq birthday (substring birthday (match-end 0)))
          (insert
           (format "%%%%(diary-anniversary %d %d %d) %s %s (%%d years)\n"
                   day month year type (bbdb-record-name rec))))))))

(defcustom bbdb-vcard-export-dir "~/s45i/vcard"
  "VCARD entries will be exported to this directory."
  :group 'bbdb-rf
  :type 'directory)

(defun rf-bbdb-time-convert (date)
  "Converts the BBDB timestamps to the time format as produced decode-time.
BBDB does not record the exact date, but only the day of the last change."
  (let ((parts (bbdb-split date "-")))
    (list 0 0 0
          (string-to-number (caddr parts))
          (string-to-number (cadr parts))
          (string-to-number (car parts)))))
 
(defun bbdb-vcard-export ()
  "Exports currently displayed BBDB records to vcards.

This function is intended to be used with the command line tool `scmxx'
for accessing Siemens mobile phones.

cd ~/s45i/vcards
# delete existing vcards??? Really?
scmxx -r -Fall
# send new slots
scmxx -s -Fall bbdb*"
  (interactive)
  (let ((records bbdb-records)
        (num 0)
        rec addresses phones notes)
    (while records
      (setq rec (caar records)
            records (cdr records)
            addresses (mapcar (lambda (r) (cons (aref r 0) r))
                              (bbdb-record-addresses rec))
            phones (mapcar (lambda (r) (cons (aref r 0) r))
                           (bbdb-record-phones rec))
            notes (bbdb-record-raw-notes rec))
      (let (;; the following fields we need to extract
            (firstname (or (bbdb-record-firstname rec) ""))
            (lastname  (or (bbdb-record-lastname rec) ""))
            (categories (assoc 'categories notes))
            street city zip country
            home-phone work-phone cell-phone fax
            (www (assoc 'www notes))
            (net (bbdb-record-net rec))
            (timestamp (rf-bbdb-time-convert (bbdb-record-getprop rec 'timestamp)))
            ;; the timestamp will be mapped to the REVISION
            year month day hour minute second    ;; msec
            )
        ;; generate revision data
        (let ((vars '(year month day hour minute second))
              (i 5))
          (while (>= i 0)
            (eval (list 'setq (car vars) (nth i timestamp)))
            (setq vars (cdr vars) i (1- i))))
;        (setq msec (% (nth 2 (current-time)) 60))
        ;; get address
        (setq addresses (or (assoc "home" addresses)
                            (assoc "work" addresses)
                            (assoc "job" addresses)
                            (car addresses)))
        (when addresses
          (setq addresses (cdr addresses)
                street (bbdb-address-streets addresses)
                zip (bbdb-address-zip addresses)
                city (bbdb-address-city addresses)
                country (bbdb-address-country addresses)))
        ;; get phones
        (setq home-phone (or (assoc "home" phones)
                             (assoc "eltern" phones))
              home-phone (if home-phone (aref (cdr home-phone) 1))
              work-phone (or (assoc "work" phones)
                             (assoc "job" phones))
              work-phone (if work-phone (aref (cdr work-phone) 1))
              cell-phone (or (assoc "cell" phones)
                             (assoc "mobile" phones))
              cell-phone (if cell-phone (aref (cdr cell-phone) 1))
              fax (or (assoc "fax" phones)
                      (assoc "home fax" phones)
                      (assoc "work fax" phones)
                      (assoc "job fax" phones))
              fax (if fax (aref (cdr fax)  1)))
        ;;
        (setq net (if net (car net)))
        (save-excursion
          (find-file (expand-file-name (format "bbdb%03d.vcf" num)
                                       bbdb-vcard-export-dir))
          (setq num (1+ num))
          (erase-buffer)
          (insert "BEGIN:VCARD\n"
                  "VERSION:2.1\n"
                  (format "REV:%04d%02d%02dT%02d%02d\n"
                          year month day hour minute))
          (if categories
              (insert (format "CATEGORIES:%s\n"
                              (mapconcat 'identity categories ","))))
          (insert (format "FN:%s, %s\n" lastname firstname)
                  (format "N:%s;%s\n" lastname firstname)) ;
          (if (or street city zip country)
              (insert (format "ADR:;;%s;%s;;%s;%s\n"
                              (if street (mapconcat 'identity street ",") "")
                              (or city "") (or zip "") (or country ""))))
          (if home-phone
              (insert (format "TEL;HOME:%s\n" home-phone)))
          (if work-phone
              (insert (format "TEL;WORK:%s\n" work-phone)))
          (if cell-phone
              (insert (format "TEL;CELL:%s\n" cell-phone)))
          (if fax
              (insert (format "TEL;FAX:%s\n" fax)))
          (if net
              (insert (format "EMAIL;INTERNET:%s\n" net)))
          (if www
              (insert (format "URL:%s\n" (cdr www))))
          (insert "END:VCARD\n")
          (save-buffer)
          (kill-buffer (current-buffer))
          )))))

(defcustom bbdb/vm-pop-up-bbdb-buffer-on-vm-system-state t
  "Pop up the BBDB buffer only for this state.
The variable content will be evaluated and should be non-nil in order to
pop-up the BBDB buffer."
  :group 'bbdb-mua-specific-vm
  :type '(choice (const :tag "Always"
                        t)
                 (const :tag "Not previewing"
                        (not (eq vm-system-state 'previewing)))
                 (const :tag "Showing"
                        (eq vm-system-state 'showing))
                 ))

(defun bbdb/vm-pop-up-bbdb-buffer-hook ()
  "Popup the BBDB buffer only for selected VM states.

 This requires a modified version of VM offering the `vm-showing-message-hook'
 After calling insinuate you should have the next two lines.

   (remove-hook 'vm-select-message-hook 'bbdb/vm-pop-up-bbdb-buffer)
   (add-hook 'vm-showing-message-hook  'bbdb/vm-pop-up-bbdb-buffer-hook)

 The BBDB buffer will now only popup if you are showing a message but not if
 you are only previewing it.  This might enable faster browsing of messages."
  (vm-select-folder-buffer)
  ;; TODO remove this line 
  (message "%S %s" vm-system-state bbdb/vm-pop-up-bbdb-buffer-on-vm-system-state)
  (if (eval bbdb/vm-pop-up-bbdb-buffer-on-vm-system-state)
      (bbdb/vm-pop-up-bbdb-buffer)))


(defun bbdb-yank-ccc ()
  (interactive)
  (require 'message)
  (let ((ccc (save-excursion
               (set-buffer bbdb-buffer-name)
               (and bbdb-records
                    (car bbdb-records)
                    (bbdb-record-getprop (caar bbdb-records) 'gnus-folder)))))
    (if ccc
        (message-carefully-insert-headers (list (cons 'Ccc ccc))))))



(provide 'bbdb-rf)

;;; bbdb-rf.el ends here
