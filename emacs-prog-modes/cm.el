;;; cm.el: code metric warning infrastructure for emacs.

;; $Id: cm.el,v 1.6 2001/09/10 10:16:24 burton Exp $

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: clip save text
;; Version: 1.0.0

;; This file is [not yet] part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; cm.el is an Emacs package which provides an infrastructure (and default
;; metrics) for signaling poor code.
;;
;;; Writing new cm functions:
;; 
;; In order to write new code metrics just create a normal lisp function which
;; calls cm-report-warning when there is a problem.  You need to call
;; cm-register-warning so that your function is registered.  After this
;; everything should work fine.  The function `cm-process-current-buffer' will
;; be called and reports any warnings it receives.  Geneally your function
;; should not have any side effects which set the region/point, monkey with the
;; kill-ring, etc.
;;
;;; Notation:
;;
;; The based package/namespace cm provides the infrastructure for everything.
;; By default it provides multiple test cases.  These are in the cm-test-warning
;; or cm-test-debug.  The warning namespace is used to display coding problems.
;; The debug namespace is used to determine overview information (num lines of
;; code, comments per LOC, etc).
;;

;;; TODO:
;; 
;; - ability for a 'click here to help resolve' message.
;;
;; - provide support for 'checkdoc' style buffer fixes.  
;; 
;; - Only issue a warning with find-file if one or more metric problems are found.
;;
;; - Try to minimize interactive functions
;;
;; - fill the WARNING: explanation and resolution lines.
;;
;; - add support for running across a directory
;;
;; - add some more default metrics:
;;
;;    - number of comments per line.
;;
;;    - how do I break on methods??
;;
;;    - maximum number of FIXME/FIX ME/HACK lines. 
;;
;;    - maximum number of lines in the ENTIRE file
;;
;;    - cyclomatic complexity
;;          http://hissa.nist.gov/HHRFdata/Artifacts/ITLdoc/235/sttoc.htm

(defvar cm-buffer "*Code Metric Warning*"
  "Temp buffer used when reporting errors.")

(defvar cm-current-buffer nil "Buffer that needs to be analyzed.")

(defvar cm-p nil
  "True when warnings were issued for the current buffer.")
(make-variable-buffer-local 'cm-p)

(defvar cm-warning-functions '() "List of functions to call when processing buffers.")

(defvar cm-warning-count 0 "Number of warnings reported for the current file.")

(defvar cm-showing-warnings nil
  "True if the user is currently being shown a code metric warning.")
(make-variable-frame-local 'cm-showing-warnings)

(defface cm-warning-face '((t (:foreground "yellow" :italic t)))
  "Face used to highlight warnings.")

;; (defadvice delete-other-windows(after cm-keep-warning-buffer())
;;   "Make sure to always keep a copy of the warning buffer if it is needed."
;;   ;;FIXME: if the current buffer is the same it is always kept... make sure to
;;   ;;delete it if this is called interactively.. 

;;   ;;It is also important that when we switch to another buffer that we also call
;;   ;;this advice...
  
;;   (if (not (interactive-p))
;;       (cm-show-warnings)))

;; (ad-activate 'delete-other-windows)

(defun cm-process-current-buffer() 
  "Process the current buffer and warn of any problems."
  (interactive)

  (setq cm-warning-count 0)
  
  (setq cm-current-buffer (current-buffer))
  
  (cm-init-buffer)

  ;;go over all registered functions
  (let(function file-regexp i element)

    (setq i 0)
    (while (< i (safe-length cm-warning-functions))
      (setq element (nth i cm-warning-functions))
      (setq function (car element))
      (setq file-regexp (nth 1 element))

      (if (functionp function)
          (progn 

            ;;if the regexp matches call the function
            (if (or (null file-regexp)
                    (string-match file-regexp (buffer-file-name cm-current-buffer)))
                (save-excursion
                  ;;set the buffer that needs to be used
                  (set-buffer cm-current-buffer)

                  (funcall function)))))
      (setq i (1+ i))))

  
  ;;let the user know how many warnings were reported
  (set-buffer cm-buffer)
  (beginning-of-buffer)
  (forward-line 2)
  (insert (format "-- Warnings reported: %i" cm-warning-count))

  ;;make sure our lines aren't too long
  (cm-fill-warning-buffer)

  ;;goto the head of the class... do not pass go... do not collect $200 dollars!
  (toggle-read-only 1)
  (beginning-of-buffer)

  ;;report any warnings!
  (set-buffer cm-current-buffer)

  ;;(view-buffer cm-current-buffer)
  (cm-show-warnings)
  
  ;;reset warnings
  (setq cm-p nil))

;;(defun cm-

(defun cm-show-warnings()
  "If any warnings were detected or the last warning was for the current
buffer, show the warnings."

  (shrink-window-if-larger-than-buffer (display-buffer cm-buffer)))

(defun cm-fill-warning-buffer()
  "Fill lines in the output buffer"

  (cm-fill-regexp "^WARNING:" "        ")
  (cm-fill-regexp "^\tExplanation: " "\t             ")
  (cm-fill-regexp "^\tResolution: " "\t            "))

(defun cm-fill-regexp(regexp fill-prefix)
  
  
  (set-buffer cm-buffer)

  (let(fill-column)
    (setq fill-column (window-width))
    (while (re-search-forward regexp nil t)
      (save-excursion
        (let(begin end)
          (beginning-of-line)
          (setq begin (point))
          (end-of-line)
          (setq end (point))
          
          (fill-region begin end))))))

(defun cm-init-buffer()
  "Init the warning buffer, creating it and erasing it if necessary."

  (set-buffer (get-buffer-create cm-buffer))
  (toggle-read-only -1)
  (cm-mode)
  
  (erase-buffer)

  (insert (format "-- Code Metric Report generated on %s\n" (current-time-string)))
  (insert (format "-- File: %s\n" (buffer-file-name cm-current-buffer)))
  (insert "\n\n"))
  

(defun cm-report-warning(warning &optional explanation resolution)
  "Report, in the code metric buffer, a warning and provide a correct
explanation of why the warning was generated."
  (setq cm-p t)
  (setq cm-warning-count (1+ cm-warning-count))
  
  (set-buffer (get-buffer-create cm-buffer))

  (insert (format "WARNING: %s\n\n" warning))
  (if explanation
      (insert (format "\tExplanation: %s\n\n" explanation)))
  (if resolution
      (insert (format "\tResolution: %s\n\n" resolution))))

(defun cm-register-warning(function &optional file-regexp)
  "Register a warning function."
  
  (add-to-list 'cm-warning-functions (list function file-regexp)))

(defun cm-mode()
  "Turn on `cm-mode'"
  (interactive)
  
  (kill-all-local-variables)
  ;;(use-local-map cvs-annotate-mode-map)

  (setq major-mode 'cm-mode)
  (setq mode-name "Metrics")
  (font-lock-mode 1))

;;register trigger functions

;;FIXME: We can't call this function after find-file... 
;;(add-hook 'find-file-hooks 'cm-process-current-buffer)
;;(add-hook 'after-revert-hook 'cm-process-current-buffer)

;;add font-lock stuff
(font-lock-add-keywords 'cm-mode '(("\\(^WARNING:.*\\)" 1 'cm-warning-face keep)))

;;add comments 
(font-lock-add-keywords 'cm-mode '(("\\(^--.*\\)" 1 'font-lock-comment-face keep)))

;;highlight bold keywords...
(font-lock-add-keywords 'cm-mode '(("\\(^\t[A-Za-z]+:\\)" 1 'bold keep)))


;;load default code metrics.
(require 'cm-cvs)
(require 'cm-lines)
(require 'cm-fixme)

(provide 'cm)

