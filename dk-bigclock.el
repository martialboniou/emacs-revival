;;; dk-bigclock.el --- Display the current time with large numbers.

;;; Copyright (C) 2001 by Daisuke Kakura

;; Author: Daisuke Kakura <info"AT"kakura.jp>
;; Maintainer: Martial Boniou <hondana@gmx.com>
;; Created: 2001-02-07
;; Version: 1.0-dk
;; Info: was bigclock.el

;; Changelog: rename bigclock to dk-bigclock to avoid errors with speedbar (2013-06-24)
;;            add Custom variables and insert autoload tag (2013-06-26)

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.       If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides the commands `dk-bigclock' to display the
;; current time with large numbers.

;;; Installation

;; Put this file on your Emacs-Lisp load path and add following into
;; your ~/.emacs startup file
;;
;;        (autoload 'dk-bigclock "dk-bigclock" "Display dk-bigclock window.")
;;        (custom-set-variables
;;         '(dk-bigclock-jpn t)) ; for Japanese users.

;;; Support

;; This program is available at <http://...>. Any comments,
;; suggestions, bug reports or upgrade requests are welcome.  Please
;; send them to Daisuke Kakura <info"AT"kakura.jp>.

;;; Change Log:

;; Version 1.0   2001-02-07 Daisuke Kakura
;;              Initial revision.

;;; Code:

(require 'cl)

;; user defined variables.

(defgroup dk-bigclock nil
  "Display the current time with large numbers."
  :group 'tools)

(defcustom dk-bigclock-buffer-name "*dk-bigclock*"
  "Name of the dk-bigclock buffer."
  :type 'string
  :group 'dk-bigclock)

(defcustom dk-bigclock-quit-delay 3
  "Time in secounds to delay before quitting dk-bigclock."
  :type 'integer
  :group 'dk-bigclock)

(defcustom dk-bigclock-margin ""
  "Margin on left."
  :type 'string
  :group 'dk-bigclock)

(defcustom dk-bigclock-24hour nil
  "Display time in 24-hour format."
  :type 'boolean
  :group 'dk-bigclock)

(defcustom dk-bigclock-no-zero t
  "t to remove first digit 0."
  :type 'boolean
  :group 'dk-bigclock)

(defcustom dk-bigclock-no-seconds nil
  "t to remove seconds."
  :type 'boolean
  :group 'dk-bigclock)

(defcustom dk-bigclock-jpn nil
  "Use Japanese font."
  :type 'boolean
  :group 'dk-bigclock)

(defvar bcns-japanese
  '("┏━━━┓" "    ┓   " "┏━━━┓" "┏━━━┓" "┓      ┓" "┏━━━┓" "┏━━━┓" "┏━━━┓" "┏━━━┓" "┏━━━┓"
    "┃      ┃" "    ┃   " "        ┃" "        ┃" "┃      ┃" "┃        " "┃        " "        ┃" "┃      ┃" "┃      ┃"
    "┃      ┃" "    ┃   " "        ┃" "        ┃" "┃      ┃" "┃        " "┃        " "        ┃" "┃      ┃" "┃      ┃"
    "┃      ┃" "    ┃   " "┏━━━┛" "┣━━━┫" "┃      ┃" "┗━━━┓" "┣━━━┓" "        ┃" "┣━━━┫" "┃      ┃"
    "┃      ┃" "    ┃   " "┃        " "        ┃" "┗━━━┫" "        ┃" "┃      ┃" "        ┃" "┃      ┃" "┗━━━┫"
    "┃      ┃" "    ┃   " "┃        " "        ┃" "        ┃" "        ┃" "┃      ┃" "        ┃" "┃      ┃" "        ┃"
    "┃      ┃" "    ┃   " "┃        " "        ┃" "        ┃" "        ┃" "┃      ┃" "        ┃" "┃      ┃" "        ┃"
    "┗━━━┛" "    ┗   " "┗━━━┛" "┗━━━┛" "        ┗" "┗━━━┛" "┗━━━┛" "        ┗" "┗━━━┛" "┗━━━┛"))

(defvar bcds-japanese
  '("      "
    "      "
    "  ・  "
    "      "
    "      "
    "  ・  "
    "      "
    "      "))

(defvar bcns-ascii
  '("+-------+ " "    +     " "+-------+ " "+-------+ " "+       + " "+-------+ " "+-------+ " "+-------+ " "+-------+ " "+-------+ "
    "|       | " "    |     " "        | " "        | " "|       | " "|         " "|         " "        | " "|       | " "|       | "
    "|       | " "    |     " "        | " "        | " "|       | " "|         " "|         " "        | " "|       | " "|       | "
    "|       | " "    |     " "+-------+ " "+-------+ " "|       | " "+-------+ " "+-------+ " "        | " "+-------+ " "|       | "
    "|       | " "    |     " "|         " "        | " "+-------+ " "        | " "|       | " "        | " "|       | " "+-------+ "
    "|       | " "    |     " "|         " "        | " "        | " "        | " "|       | " "        | " "|       | " "        | "
    "|       | " "    |     " "|         " "        | " "        | " "        | " "|       | " "        | " "|       | " "        | "
    "+-------+ " "    +     " "+-------+ " "+-------+ " "        + " "+-------+ " "+-------+ " "        + " "+-------+ " "+-------+ "))

(defvar bcds-ascii
  '("      "
    "      "
    "  *   "
    "      "
    "      "
    "  *   "
    "      "
    "      "))

(defvar dk-bigclock-timer-handle nil "Handle for the update timer.")
(defvar dk-bigclock-window nil "Pointer to the dk-bigclock window.")
(defvar dk-bigclock-toggle-stay nil "Keep displaying dk-bigclock or quit after `dk-bigclock-quit-delay'.")

(defun nthcar (n l) "car of nthcdr"
  (car (nthcdr n l)))

;;;###autoload
(defun dk-bigclock () "Display dk-bigclock window."
  (interactive)
  (setq bcns bcns-ascii
        bcds bcds-ascii)
  (if dk-bigclock-jpn
      (setq bcns bcns-japanese
            bcds bcds-japanese))
  (let ((window-min-height 9)
        (old-window (selected-window)))
    (if (window-live-p dk-bigclock-window) ; dk-bigclock stil exists.
        (if dk-bigclock-toggle-stay
            (setq dk-bigclock-toggle-stay nil)
          (setq dk-bigclock-toggle-stay t))
      (setq dk-bigclock-window (split-window-vertically (- (window-height) 9))))
    (setf (selected-window) dk-bigclock-window)
    (setq dk-bigclock-buffer (switch-to-buffer dk-bigclock-buffer-name))
    (unless dk-bigclock-timer-handle
      (setq dk-bigclock-timer-handle (run-at-time nil 1 #'dk-bigclock-display)))
    (setf (selected-window) old-window)
    (unless dk-bigclock-toggle-stay
      (if (sit-for dk-bigclock-quit-delay)
          (dk-bigclock-quit)))))

(defun dk-bigclock-display () "Update the dk-bigclock window."
  (with-current-buffer dk-bigclock-buffer
    (let ((buffer-read-only nil))
      (setf (buffer-string) "")
                                        ; get current time and chop it into 6 digits.
      (setq now (decode-time))
      (setq fixed-third (third now))
      (if (and (not dk-bigclock-24hour) (> (third now) 12))
          (setq fixed-third (- (third now) 12)))
      (setq d1 (/ fixed-third  10)
            d2 (- fixed-third        (* d1 10))
            d3 (/ (second now) 10)
            d4 (- (second now) (* d3 10))
            d5 (/ (first now)        10)
            d6 (- (first now)        (* d5 10)))
                                        ; prepare first digit and last two digits(seconds).
      (if (and dk-bigclock-no-zero (= d1 0))
          (setq d100 "          "
                d110 "          "
                d120 "          "
                d130 "          "
                d140 "          "
                d150 "          "
                d160 "          "
                d170 "          ")
        (setq d100 (nthcar d1 bcns)
              d110 (nthcar (+ d1 10) bcns)
              d120 (nthcar (+ d1 20) bcns)
              d130 (nthcar (+ d1 30) bcns)
              d140 (nthcar (+ d1 40) bcns)
              d150 (nthcar (+ d1 50) bcns)
              d160 (nthcar (+ d1 60) bcns)
              d170 (nthcar (+ d1 70) bcns)))
      (if dk-bigclock-no-seconds
          (setq dot0 ""
                dot1 ""
                dot2 ""
                dot3 ""
                dot4 ""
                dot5 ""
                dot6 ""
                dot7 ""
                d500 ""
                d510 ""
                d520 ""
                d530 ""
                d540 ""
                d550 ""
                d560 ""
                d570 ""
                d600 ""
                d610 ""
                d620 ""
                d630 ""
                d640 ""
                d650 ""
                d660 ""
                d670 "")
        (setq dot0 (nthcar 0 bcds)
              dot1 (nthcar 1 bcds)
              dot2 (nthcar 2 bcds)
              dot3 (nthcar 3 bcds)
              dot4 (nthcar 4 bcds)
              dot5 (nthcar 5 bcds)
              dot6 (nthcar 6 bcds)
              dot7 (nthcar 7 bcds)
              d500 (nthcar d5 bcns)
              d510 (nthcar (+ d5 10) bcns)
              d520 (nthcar (+ d5 20) bcns)
              d530 (nthcar (+ d5 30) bcns)
              d540 (nthcar (+ d5 40) bcns)
              d550 (nthcar (+ d5 50) bcns)
              d560 (nthcar (+ d5 60) bcns)
              d570 (nthcar (+ d5 70) bcns)
              d600 (nthcar d6 bcns)
              d610 (nthcar (+ d6 10) bcns)
              d620 (nthcar (+ d6 20) bcns)
              d630 (nthcar (+ d6 30) bcns)
              d640 (nthcar (+ d6 40) bcns)
              d650 (nthcar (+ d6 50) bcns)
              d660 (nthcar (+ d6 60) bcns)
              d670 (nthcar (+ d6 70) bcns)))
                                        ; it's show time!
      (setf (buffer-string)
            (format (concat "%s%s%s%s%s%s%s%s%s\n%s%s%s%s%s%s%s%s%s\n%s%s%s%s%s%s%s%s%s\n%s%s%s%s%s%s%s%s%s\n"
                            "%s%s%s%s%s%s%s%s%s\n%s%s%s%s%s%s%s%s%s\n%s%s%s%s%s%s%s%s%s\n%s%s%s%s%s%s%s%s%s\n")
                    dk-bigclock-margin d100 (nthcar d2 bcns) (nthcar 0 bcds) (nthcar d3 bcns) (nthcar d4 bcns) dot0 d500 d600
                    dk-bigclock-margin d110 (nthcar (+ d2 10) bcns) (nthcar 1 bcds) (nthcar (+ d3 10) bcns) (nthcar (+ d4 10) bcns) dot1 d510 d610
                    dk-bigclock-margin d120 (nthcar (+ d2 20) bcns) (nthcar 2 bcds) (nthcar (+ d3 20) bcns) (nthcar (+ d4 20) bcns) dot2 d520 d620
                    dk-bigclock-margin d130 (nthcar (+ d2 30) bcns) (nthcar 3 bcds) (nthcar (+ d3 30) bcns) (nthcar (+ d4 30) bcns) dot3 d530 d630
                    dk-bigclock-margin d140 (nthcar (+ d2 40) bcns) (nthcar 4 bcds) (nthcar (+ d3 40) bcns) (nthcar (+ d4 40) bcns) dot4 d540 d640
                    dk-bigclock-margin d150 (nthcar (+ d2 50) bcns) (nthcar 5 bcds) (nthcar (+ d3 50) bcns) (nthcar (+ d4 50) bcns) dot5 d550 d650
                    dk-bigclock-margin d160 (nthcar (+ d2 60) bcns) (nthcar 6 bcds) (nthcar (+ d3 60) bcns) (nthcar (+ d4 60) bcns) dot6 d560 d660
                    dk-bigclock-margin d170 (nthcar (+ d2 70) bcns) (nthcar 7 bcds) (nthcar (+ d3 70) bcns) (nthcar (+ d4 70) bcns) dot7 d570 d670)))))

(defun dk-bigclock-quit () "Close the dk-bigclock."
  (interactive)
  (when dk-bigclock-timer-handle
    (cancel-timer dk-bigclock-timer-handle)
    (setq dk-bigclock-timer-handle nil))
  (kill-buffer dk-bigclock-buffer)
  (delete-window dk-bigclock-window)
  (setq dk-bigclock-toggle-stay nil))

(provide 'dk-bigclock)
;;; dk-bigclock.el ends here.
