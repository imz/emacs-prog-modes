;;; code-review-request.el --- prepare version control diffs for mailing

;; Copyright (C) 2001 Jeffrey Seifert

;; Author: Jeffrey Seifert (seifert@everybody.org)
;; Maintainer: Jeffrey Seifert (seifert@everybody.org)
;; Keywords:
;; Version: 0.1

;; This file is not (yet) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; These routines provide a convenient method to send out diffs of
;; documents under version-control via e-mail for the purposes of code
;; review.  The final outgoing e-mail will be send in multi-part MIME
;; format, with both a standard text version and an HTML color-coded
;; version.
;;
;; To prepare a non-contextual diff on the file in the current buffer,
;; use
;;      M-x code-review-request-nocontext
;; In a similar vein,
;;      M-x code-review-request-context
;; will prepare a contextual diff.  Both commands will create a
;; mail buffer in an alternate window with the diff inside and the
;; filename in the subject heading.  If the optional variable
;; 'code-review-request-to' is set, the 'To:' field will contain the
;; value of this variable.
;;
;; If a mail buffer already exists, invoking either the contextual or
;; non-contextual diff commands will append the diff to the end of the
;; current mail buffer and append the filename to the end of the
;; subject line.  In this manner, you can include diffs for several
;; files within the same mail message.
;;
;; This has been tested with Emacs 20.7 and XEmacs 21, using the CVS
;; version control system.

;;; Install:

;; In your .emacs add the line
;;     (require 'code-review-request)
;;
;; To assign keyboard shortcuts of 'C-c r' for a non-contextual diff
;; and 'C-c C-r' for a contextual diff with these lines in .emacs:
;;     (global-set-key "\C-cr" 'code-review-request-nocontext)
;;     (global-set-key "\C-cC-r" 'code-review-request-context)

;;; History:
;; 
;; Sat Feb 24 22:48:43 2001 (seifert):  init


(defvar code-review-request-to nil
  "Comma separated string of default e-mail addresses to mail code
reviews to.")

(defun code-review-request-nocontext ()
  "Prepares an e-mail message to be sent out showing a non-contextual
diff of the current buffer"
  (interactive)
  (code-review-request ""))

(defun code-review-request-context ()
  "Prepares an e-mail message to be sent out showing a contextual diff
of the current buffer"
  (interactive)
  (code-review-request "-c"))

(defun code-review-request (diff-switches)
  (require 'vc)
  (when (buffer-modified-p)
    (progn
      (ding)
      (when (y-or-n-p "Buffer has been modified without saving; save before performing diff? ")
	(save-buffer))))
  (if (not (vc-backend (buffer-file-name)))
      (error "File %s is not under version control" (buffer-file-name))
    (let ((code-review-filename (buffer-name)))
      (message "Getting diff for %s..." code-review-filename)
      (let ((vc-diff-switches diff-switches))  ; Hack for XEmacs version of vc
	(vc-backend-diff (buffer-file-name)))
      (set-buffer "*vc-diff*")
      (if (= 0 (buffer-size))
	  (message "No changes to %s since latest version" code-review-filename)
	(let ((code-review-mail-buffer (get-buffer "*mail*")))
	  (if (and (bufferp code-review-mail-buffer) (buffer-modified-p code-review-mail-buffer))
	      (let ((mail-buffer-window (get-buffer-window code-review-mail-buffer)))
		(progn
		  (if (windowp mail-buffer-window)
		      (select-window mail-buffer-window)
		    (pop-to-buffer code-review-mail-buffer))
		  (goto-char (point-max))
		  (insert-buffer "*vc-diff*")
		  (open-line 3)
 		  (mail-subject)
 		  (insert ", " code-review-filename)
		  (mail-text)
		  ))
	      (compose-mail code-review-request-to
			    (concat "[CR] " code-review-filename)
			    `(("cc" . ,user-mail-address))
			    )
	      (save-excursion
		(add-hook 'mail-send-hook 'code-review-request-mimeify)
		(mail-text)
		(insert-buffer "*vc-diff*")
		(open-line 3))
	      (message "Press C-c C-c when done to send the message.")
	    ))))))

(defvar code-review-request-mime-separator "mysteryboxofun"
  "String to use as a MIME separator")

(defun code-review-request-mimeify ()
  (let ((body (buffer-substring (mail-text-start) (point-max)))
	(title (mail-fetch-field "Subject")))
    (goto-char (mail-header-end))
    (insert (concat "Mime-Version: 1.0\n"
		     "Content-Type: multipart/alternative; boundary="
		     code-review-request-mime-separator
		     "\n"))
    (goto-char (mail-text-start))
    (insert (concat "This is a multipart message in MIME format.\n\n"
		    "--" code-review-request-mime-separator "\n"
		    "Content-Type: text/plain; charset=\"iso-8859-1\"\n"
		    "Content-Transfer-Encoding: 8bit\n\n"))
    (goto-char (point-max))
    (insert (concat "\n\n--" code-review-request-mime-separator "\n"
		    "Content-Type: text/html; charset=\"iso-8859-1\"\n"
		    "Content-Transfer-Encoding: 8bit\n\n"))
    (code-review-request-htmlize-text title body)
    (goto-char (point-max))
    (insert (concat "\n\n--" code-review-request-mime-separator "--\n\n"))
    )
  (remove-hook 'mail-send-hook 'code-review-request-mimeify)
)

(defvar code-review-htmlize-list
  '(("^\\(!.*\\)$" "#777777")                                ; changed code
    ("^\\(\\(\\+\\|&gt;\\) .*\\)$" "#00688B")                ; added code
    ("^\\(\\(-\\|&lt;\\) .*\\)$" "#8B1C62")                  ; removed code
    ("^\\(Index: [A-za-z0-9_~.-]+\\|=+\\)$" "#005500")       ; index
    ("^\\(RCS file: [A-za-z0-9_~./-]+,v\\|retrieving revision [0-9.]+\\|diff .* [A-za-z0-9_~.-]+\\)"
     "#00BB00")                                              ; index part 2
    ("^\\(\\*+\\|\\*\\*\\* [0-9]+,[0-9]+ \\*\\*\\*\\*\\|--- [0-9]+,[0-9]+ ----\\|[0-9,]+[acd][0-9,]+\\)$"
     "#B77C22")                                              ; hunk header
    ))

(defun code-review-request-htmlize-text (title text)
  (let ((currbuffer (current-buffer))
	(tempbuffer (generate-new-buffer "*code-review*")))
    (set-buffer tempbuffer)
    (insert text)
    (goto-char (point-min))
    (perform-replace "&" "&amp;" nil nil nil)
    (goto-char (point-min))
    (perform-replace "<" "&lt;" nil nil nil)
    (goto-char (point-min))
    (perform-replace ">" "&gt;" nil nil nil)
    (dolist (fontsubst code-review-htmlize-list)
      (let ((fontregexp (car fontsubst))
	    (fontcolor (nth 1 fontsubst)))
	(goto-char (point-min))
	(while (re-search-forward fontregexp nil t)
	  (replace-match
	   (concat "<font color=\"" fontcolor "\">\\1</font>")))))
    (html-word-wrap)
    (goto-char (point-min))
    (insert (concat "<HTML>\n<HEAD><TITLE>" title "</TITLE></HEAD>\n"
		    "<BODY bgcolor=\"#ffffff\">\n<pre>\n"))
    (goto-char (point-max))
    (insert "\n</pre>\n</BODY></HTML>\n")
    (set-buffer currbuffer)
    (insert-buffer tempbuffer)
    (kill-buffer tempbuffer)
    ))

(defun html-word-wrap ()
  (interactive)
  (goto-char (point-min))
  (let ((num-of-lines (count-lines (point-min) (point-max)))
	(in-htmltag 1)
	(colcounter 1)
	(rowcounter 0))
    (while (< rowcounter num-of-lines)
      (setq colcounter 1)
      (while (not (eolp))
	(if (eq (char-after) ?<)
	    (setq in-htmltag 0)
	  (if (eq (char-after) ?>)
	      (setq in-htmltag 1)
	    ;; If entity is an escape character, treat as only one character
	    (setq colcounter (+ colcounter in-htmltag))
	    (when (= colcounter 80)
	      (insert "<font color=black>&#187;</font>\n")
	      (setq colcounter 2))))
	    (if (eq (char-after) ?&)
		(search-forward ";")
	      (forward-char)))
      (forward-line)
      (setq rowcounter (+ 1 rowcounter)))))

(provide 'code-review-request)
;;; code-review-request.el ends here

