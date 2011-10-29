;;; wl-highlight-ad.el --- Highlights advertisements.
;; Copyright (C) 2003 Daisuke Kakura <info"AT"kakura.jp>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;	 Tired of advertisements in email? Try this.

;;; Install:
;;
;;	 (require 'wl-highlight-ad)
;;
;;	 For example:
;;
;;	 I set `wl-highlight-ad-regexp-user-alist' like this in my .emacs.
;;
;;	 (setq wl-highlight-ad-regexp-user-alist
;;		 '(("@.*nikkeibp.co.jp"	 ("^ÑüPRÑü+$"					  . "^Ñü+$") default)
;;			("@egroups.co.jp"		 ("^-.*~-->$"					  . "^-+~->$"))
;;			("@mainichi.co.jp"	 ("^-+ÅyÇ`ÇcÅz-+$"
;;				. "\n.*\n.*\n.*\n.*\n.*\n-+\n.*\n.*\n.*\n.*\n.*\n-+\\|\n.*\n.*\n.*\n.*\n.*\n-+"))
;;			("@ascii24.com"		 ("^Ñü\\( ASCII\\)? PR Ñü+$" . "Ñü PR Ñü$"))
;;			("@pc.mycom.co.jp"	 ("^Å\+\\[PR\\]Å\+$"			  . "^Å\+$"))
;;			("0000018894"			 ("^Ñ™PRÑ™+$"					  . "^Ñ™+$")
;;										 ("^-Åy.*Ç‹ÇÆ.*Åz-+$"		  . "^-+$")
;;										 ("^_@_+-PR-_+$"				  . "^_+$"))
;;			("0000037616"			 ignore)
;;			("@tegami.com"			 ("^-Åy.*Ç‹ÇÆ.*Åz-+$"		  . "^-+$")
;;										 ("^_@_+-PR-_+$"				  . "^_+$") default)
;;			("@hotmail.com"		 ("^_+\n.*MSN"					  . "^$")	continue)
;;			("@yahoo.co"			 ("^_+\nDo You Yahoo!\\?"	  . "^$")	continue)
;;			(".*"						 ("^\\[snip!\\]$"				  . "")		default)))
;;
;;
;;	 This list must be like this...
;;
;;	 '(("HEADER-REGEXP" ("AD-START-REGEXP" . "AD-END-REGEXP")
;;							  ("AD-START-REGEXP" . "AD-END-REGEXP") OPTION))
;;
;;	 `HEADER-REGEXP' is regexp to search for header lines. If you want
;;	 it to search only From: header, you can do like this,
;;
;;		 "^From: .*some@address".
;;
;;	 `AD-START-REGEXP' and `AD-END-REGEXP' are regexp to search start
;;	 and end points of advertisements in the mail body part. You can
;;	 put more than one pairs like example above.
;;
;;	 `OPTION' is to specify an action to do when one of HEADER-REGEXP
;;	 matches something.
;;
;;  The searching starts from the top of the list and stops if one of
;;  HEADER-REGEXP is found. If you put `continue' option, it continues
;;  searching for the next. `default' stops searching and uses
;;  `wl-highlight-ad-regexp-default-alist' to search for more ads.
;;  `ignore' stops searching, but it doesn't highlight anything. Note
;;  that when you use `ignore', you can not have any
;;	 AD-START-REGEXP and AD-END-REGEXP pair. These options won't work
;;	 together.
;;
;;	 If none of HEADER-REGEXP was found, it will use
;;	 `wl-highlight-ad-regexp-default-alist' and search for ads. So,
;;	 `wl-highlight-ad-regexp-default-alist' works for any other email
;;	 not listed in `wl-highlight-ad-regexp-user-alist'. If you don't
;;	 like this and don't want to highlight email you have not specified
;;	 in your list, then set `wl-highlight-ad-regexp-default-alist' as
;;	 nil.
;;
;;	 If you want to add some AD-START-REGEXP and AD-END-REGEXP pairs to
;;	 `wl-highlight-ad-regexp-default-alist', you can do it like the
;;	 last element in the example above.
;;
;;	 Your comments, suggestions and bug reports are welcome.

;;; History:
;;	 2003-11-16	 v0.3	 Daisuke Kakura <info"AT"kakura.jp>
;;							 FROM-HEADER-REGEXP has been changed to
;;							 HEADER-REGEXP. Now HEADER-REGEXP matches to any
;;							 header lines.
;;	 2003-11-05	 v0.2	 Daisuke Kakura <info"AT"kakura.jp>
;;							 Added wl-highlight-ad-max-lines variable and
;;							 ignore option.
;;	 2003-11-04	 v0.1	 Daisuke Kakura <info"AT"kakura.jp> Created.

;;; Code:
;;

(defvar wl-highlight-ad t
	"Highlight advertisements if t, otherwise do nothing.")

(defvar wl-highlight-ad-regexp-default-alist
	'((".*" ("^[-=Å\ÑüÑ™]+[][ Å@Å•ÅôÅ·Å‚ÅÉÅÑÇoÇqPRAD<>]+[-=Å\ÑüÑ™]+$"
	 . "^[-=Å\ÑüÑ™]+[][ Å@Å£ÅôÅ·Å‚ÅÉÅÑÇoÇqPRAD<>]+[-=Å\ÑüÑ™]+$")))
	"The top element of `wl-highlight-ad-regexp-default-alist' must be `.*'
to match any header line.")

(defvar wl-highlight-ad-regexp-user-alist nil
	"A list of advertisements you want to highlight.")

(defvar wl-highlight-ad-max-lines 20
	"Limit length (lines) of ads. If number of lines of searched ad exceeds
this, it probably failed searching and trying to highlight wrong part.
So this is not to highlight wrong part.")

(wl-defface wl-highlight-ad-face
	'((((class color)
		 (background dark))
		(:foreground "gray35"))
	  (((class color)
		 (background light))
		(:foreground "gray35")))
	"Face used for displaying advertisements."
	:group 'wl-message-faces
	:group 'wl-faces)

(defun wl-highlight-ad-search ()
	(let ((regexp-alist (append wl-highlight-ad-regexp-user-alist
				 wl-highlight-ad-regexp-default-alist))
			 regexp-a regexp
			 (finishp nil)
			 beg end)
		(save-excursion
			(while (and (setq regexp-a (pop regexp-alist)) (not finishp))
				(goto-char (point-min))
				(re-search-forward "^$" nil t)
				(when (re-search-backward (pop regexp-a) (point-min) t 1)
					(setq finishp t)
					(while (setq regexp (pop regexp-a))
						(cond ((eq regexp 'default)
								 (setq regexp-alist wl-highlight-ad-regexp-default-alist)
								 (setq finishp nil))
								((eq regexp 'continue)
								 (setq finishp nil))
								((eq regexp 'ignore)
								 ); do nothing and finish.
								(t
								 (goto-char (point-min))
								 (while (re-search-forward (car regexp) (point-max) t 1)
									 (setq beg (match-beginning 0))
									 (re-search-forward (cdr regexp) (point-max) t 1)
									 (setq end (match-end 0))
									 (if (<= (count-lines beg end) wl-highlight-ad-max-lines)
										 (put-text-property beg end 'face 'wl-highlight-ad-face)))))))))))

(if wl-highlight-ad
	(add-hook 'mime-view-mode-hook 'wl-highlight-ad-search))

(provide 'wl-highlight-ad)

;;; wl-highlight-ad.el ends here.
