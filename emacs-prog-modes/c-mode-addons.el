;; c-mode-addons.el - some addon functions for c-mode, early version
;; Copyright (C) 1999, Avoozl en Smoke
;; last update: Sat Oct 2 19:07:30 1999

;; Tijs van Bakel: <smoke@casema.net>
;; Jorik Blaas: <jrk@vengeance.et.tudelft.nl>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This adds two hopefully useful, commands to cc-mode,
;; namely c-synopsis and c-eval-enum.
;;
;; c-synopsis shows you the synopsis for a given C command in the minibuffer
;; c-eval-enum shows you the number associated with the current enum entry
;;
;; to enable these functions:
;;
;; - generate a 'c_synopsis_list' and change the line
;;   (synopsis-file "/usr/share/emacs/20.3/etc/c_synopsis_list"))
;;   mentioned the function 'c-synopsis' to point to it.
;;   use the supplied 'parse-manpages.sh' script to generate the
;;   synopsis list.
;;
;; - copy c-mode-addons.el to one of your emacs-load-paths, or add
;;   the directory to the load-path, something like:
;;   (add-to-list 'load-path "~/c-mode-addons-0.1/")
;;
;; - put these in your .emacs:
;;
;;   (load "c-mode-addons")
;;
;;   (define-key c-mode-map "\M-s" 'c-synopsis-at-point)
;;   (define-key c++-mode-map "\M-s" 'c-synopsis-at-point)
;;
;;   (define-key c-mode-map "\C-ce" 'c-eval-enum)
;;   (define-key c++-mode-map "\C-ce" 'c-eval-enum)
;;
;;   (define-key c-mode-map "(" 'c-electric-parenthesis-open)
;;   (define-key c++-mode-map "(" 'c-electric-parenthesis-open)
;;
;; TODO
;;
;; - c-eval-enum should also handle #define's like enum { a = TEST, ..
;; - better error reporting in case something goes wrong
;; - place 'c_synopsis_list' in a better path
;; - some synopsises are too long to fit in the modeline (argH!)
;;
;; please report nasty bugs, inconveniences, hints, patches to
;; smoke@casema.net or jrk@vengeance.et.tudelft.nl

(defun c-synopsis (string)
  "lookup the synopsis for a given c function"
  (let ((start-pos 0)
       (synopsis-file "/usr/share/emacs/etc/prog-modes/c_synopsis_list"))
    (save-excursion
      (if (not (get-buffer "*synopsis*"))
	  (progn
	    (create-file-buffer "*synopsis*")
	    (set-buffer "*synopsis*")
	    (insert-file synopsis-file)
	    )
	(set-buffer "*synopsis*"))
      (goto-char (point-min))
      (if (search-forward (concat "#" string ":") (point-max) t)
	  (progn
	    (setq start-pos (point))
	    (end-of-line)
	    (message (buffer-substring start-pos (point)))))
      )))

(defun c-synopsis-at-point ()
  "lookup the synopsis for a C function at the current point"
  (interactive)
  (save-excursion
    (let ((fn (current-word)))
      (c-synopsis fn)
      )))

(defun c-electric-parenthesis-open ()
  "lookup C function and insert a '(' character"
  (interactive)
  (let ((fn (current-word)))
    (c-synopsis fn)
    (self-insert-command 1)))

(defun c-eval-enum-how-many-region (string beg end)
  "Return number of matches for REGEXP from BEG to END."
  (let ((count 0))
    (save-excursion
      (save-match-data
	(goto-char beg)
	(while (search-forward string end t)
	  (setq count (1+ count)))))
    count))

(defun c-eval-enum ()
  "Shows you what the enumeration at point evaluates to."
  (interactive)
  (let ( (origpoint 0)
	 (endwordpoint 0)
	 (basenr 0)
	 (commas 0)
	 (endpoint 0) )
    
    (save-excursion
      (setq origpoint (point))
      (c-end-of-statement)
      (setq endpoint (point))
      (goto-char origpoint)
      (if (search-forward "," endpoint 1)
	  (backward-char))
      (setq origpoint (point))
      (c-beginning-of-statement)

      (if (not (get-buffer "*c-eval-enum-buffer*"))
	  (create-file-buffer "*c-eval-enum-buffer*")
      (save-excursion
	(set-buffer "*c-eval-enum-buffer*")
	(font-lock-mode -1))
       
      (copy-to-buffer "*c-eval-enum-buffer*" (point) origpoint)
      (set-buffer "*c-eval-enum-buffer*")
      
      (goto-char (point-min))
      
      (if (not (equal major-mode 'c-mode))
	  (c-mode))
      
      (kill-comment (count-lines (point-min) (point-max) ) )
      (goto-char (point-max))
      
      (if (search-backward "=" (point-min) 1)
	  (progn
	    (forward-word 1)
	    (setq endwordpoint (point))
	    (backward-word 1)
	    (setq basenr
		  (string-to-number (buffer-substring (point) endwordpoint)))))
      (setq commas (c-eval-enum-how-many-region "," (point) (point-max)))
      (message "%d" (+ basenr commas))))))

