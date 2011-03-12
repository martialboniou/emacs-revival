;;; html-php.el --- multi-mode PHP embedded in HTML

;; Copyright (C) 2008  Dave Love

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages
;; $Revision: 2$
;; URL: http://www.loveshack.ukfsn.org/emacs

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Multiple-mode editing of PHP embedded in HTML.  Subject to the
;; limitations of multi-mode.el.  Note that there's no attempt to deal
;; with commented processing instructions markup.  Note also that this
;; only works properly with the basic HTML mode -- not PSGML or NXML.
;; Tested with the PHP mode from the hacked CC mode at the above URL.

;;; Code:

(require 'multi-mode)

(defun html-php-mode ()
  "Mode for editing PHP embedded in HTML, using multi-mode."
  (interactive)
  (set (make-local-variable 'multi-alist)
       '((html-mode)
	 (php-mode . html-php-chunk-region)))
  (multi-mode-install-modes))

(defun html-php-chunk-region (pos)
  "Mode-selecting function for PHP embedded in HTML.
See `multi-alist'."
  (let ((case-fold-search t)
	pi-start pi-end next-pi)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char pos)
	(save-excursion
	  (let* ((p1 (save-excursion
		       ;; Check whether we're on the processing
		       ;; instruction start.  Skip definitely clear of
		       ;; it and then search backwards.
		       (goto-char (min (point-max) (+ (point) 5)))
		       (search-backward "<?php" (- (point) 9) t)))
		 (match-end (if p1 (match-end 0)))
		 ;; Otherwise search backwards simply.
		 (p2 (unless p1 (search-backward "<?php" nil t))))
	    (if p2 (setq match-end (match-end 0)))
	    (setq pi-start (or p1 p2))
	    ;; Ready to search for matching terminator or next
	    ;; processing instruction.
	    (goto-char (or match-end pos)))
	  (if pi-start
	      ;; Look forward for the PI terminator.
	      (let* ((p1 (save-excursion
			   ;; Check whether we're on the terminator.
			   (backward-char 1)
			   (search-backward "?>" (- (point) 2) t)))
		     (p2 (unless p1 (search-forward "?>" nil t))))
		(setq pi-end (or p1 p2 (point-max))))
	    (goto-char pos))
	  (if (and pi-start pi-end (< pos pi-end))
	      ;; We were between PI start and terminator.
	      (list 'php-mode pi-start pi-end)
	    ;; Otherwise, look forward for a PI to delimit the HTML
	    ;; region.
	    (setq next-pi (if (search-forward "<?php" nil t)
			      (match-beginning 0)
			    (point-max)))
	    (if pi-start
		(list 'html-mode (or pi-end (point-min)) next-pi)
	      (list 'html-mode (point-min) next-pi))))))))

(provide 'html-php)
;;; html-php.el ends here
