;; c-includes -- Find all header files included by a source file
;;
;; Copyright (C) 1999 John Wiegley
;;
;; Author: John Wiegley <johnw@oneworld.new-era.com>
;; Created: 23 Mar 1999
;; Keywords: c languages oop
;; X-URL: http://oneworld.new-era.com/johnw/emacs.html

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
;;
;; After setting `c-includes-path' appropriately, you can run the
;; command `M-x c-includes' to find all of the include files that get
;; brought in by a particular source file.
;;
;; It will create a buffer called "*Includes*" that will list all of
;; the included files, nested according to their inclusion depth.
;;
;; If you provide a prefix argument to `c-includes', it will ask you
;; for a regular expression, and will display all lines along the
;; include path containing that regular expression.  Pressing C-c C-c
;; on any of those lines will go that location.
;;
;; Lastly, for the programmer in all of us, if you call `c-includes'
;; non-interactively, and pass t as the second argument, it will
;; return back to you a list of the include file names, to do with as
;; you please.  See the docstring.

;;; Changes in 1.1:
;;
;; * If a header can't be found, and has no extension, but there IS a
;;   header with an extension that has the same basename, then we
;;   assume that was the header that was meant.
;;
;; * The display is now ordered in the same way as what cpp would see,
;;   making your regexp searches more realistic.
;;
;; * Files included with double quotes are searched for first relative
;;   to the current directory.
;;
;; * `c-includes' now always asks for a regular expression, rather
;;   than relying on a prefix argument.
;;
;; * The mark gets pushed in the *Includes* buffer whenever you jump
;;   around, to enable you to get back easily.
;;
;; * When a file is included that has already been included, its name
;;   is now output, along with the line number within the *Includes*
;;   buffer where it was first included.  Pressing C-c C-c on that
;;   line will take you to the location of the first inclusion.


;;; Header:

(provide 'c-includes)

(defconst c-includes-version "1.1"
  "This version of c-includes.")
  

;;; User Variables:

(defcustom c-includes-path
  '("/usr/include" "/usr/include/sys")
  "*List of paths to search for include files."
  :type '(repeat string)
  :group 'c)

(defcustom c-includes-mode-hook nil
  "*A series of function to be run upon entering c-includes mode."
  :type 'hook
  :group 'c)


;;; User Functions:

;;;###autoload
(defun c-includes (filename &optional regexp)
  "Find all of the header files included by FILENAME.
REGEXP, if non-nil, is a regular expression to search for within
FILENAME and the files that it includes.  The output will be
structured in the same order that the compiler will see it, enabling
you determine order of occurrance."
  (interactive
   "fFind all includes brought in by: \nsSearch for regexp: ")
  (save-excursion
    (switch-to-buffer-other-window c-includes-buffer)
    (c-includes-mode)
    (delete-region (point-min) (point-max))
    (c-includes-search filename nil t 0 regexp)
    (goto-char (point-min))
    (message "Searching files...done")))


;;; Internal Functions:

(defvar c-found-paths nil
  "Internal variable used to store discovered paths.")

(defun c-includes-display-file (filename depth begd endd face)
  "Insert FILENAME into the current buffer, prettily.
DEPTH is the nesting level, while BEGD and ENDD are the beginning and
ending delimiters to put around the string.  FACE is the face to use
for the line just output."
  (save-excursion
    (set-buffer "*Includes*")
    (goto-char (point-max))
    (let ((beg (point)))
      (insert "=> " (make-string (* depth 2) ? ))
      (insert begd filename endd)
      (put-text-property beg (point) 'face face)
      (insert "\n"))
    (count-lines (point-min) (point))))
    
(defun c-includes-search (filename &optional found-paths insert
                                   depth regexp)
  "Search for all #include lines in FILENAME.
FOUND-PATHS should be a list of include files not to traverse into.
If INSERT is non-nil, then the information found by this function will
be inserted before point in the current buffer.  DEPTH is used when
inserting to keep track of the current nesting level.  REGEXP, if
non-nil, specifies a regular expression to search for within the
include files.  NOTE that only `c-includes' should set any but the
first argument."
  (if (not insert)
      (setq found-paths (cons filename
                              found-paths))
    (message "Searching file %s..." filename)
    (setq found-paths
          (cons (cons filename
                      (c-includes-display-file
                       filename depth "+" ""
                       font-lock-function-name-face))
                found-paths)))
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at
           "^\\s-*#\\s-*include\\s-*\\([\"<]\\)\\([^\">]+\\)[\">]")
          (let ((file (match-string-no-properties 2))
                (inc-type (match-string-no-properties 1)))
            (if (not (file-exists-p file))
                (let ((dirs c-includes-path))
                  (if (equal inc-type "\"")
                      (setq dirs (cons "." dirs)))
                  (while dirs
                    (let ((try (concat (car dirs) "/" file)))
                      (if (and (not (file-exists-p try))
                               (not (file-name-extension try))
                               (file-exists-p (concat try ".h")))
                          (setq file (concat try ".h")
                                dirs nil)
                        (if (file-exists-p try)
                            (setq file try
                                  dirs nil))))
                    (setq dirs (cdr dirs)))))
            (if (not (file-exists-p file))
                (if insert
                    (c-includes-display-file
                     file (and depth (1+ depth)) "?" "?"
                     font-lock-warning-face))
              (let ((prev (or (and insert
                                   (assoc file found-paths))
                              (and (not insert)
                                   (member file found-paths)))))
                (if (not prev)
                    (setq found-paths
                          (c-includes-search file found-paths
                                             insert
                                             (and depth (1+ depth))
                                             regexp))
                  (if insert
                      (c-includes-display-file
                       file (and depth (1+ depth)) "["
                       (concat "] (line "
                               (number-to-string (cdr prev)) ")")
                       font-lock-string-face))))))
        (let ((beg (point))
              (eol (save-excursion (end-of-line) (point))))
          (if (and insert regexp (> (length regexp) 0)
                   (re-search-forward regexp eol t))
              (let ((string (buffer-substring beg eol))
                    (line (1+ (count-lines (point-min) beg))))
                (save-excursion
                  (set-buffer "*Includes*")
                  (goto-char (point-max))
                  (setq beg (point))
                  (insert "   " (number-to-string line))
                  (setq eol (point))
                  (insert ":" string "\n")
                  (put-text-property beg eol 'face 'bold))))))
      (forward-line)))
  (when insert
    (c-includes-display-file filename depth "-" ""
                             font-lock-comment-face))
  found-paths)

(defvar c-includes-mode-map ())
(if c-includes-mode-map
    ()
  (setq c-includes-mode-map (make-sparse-keymap))
  (define-key c-includes-mode-map [mouse-2]
    'c-includes-mode-mouse-goto)
  (define-key c-includes-mode-map "\C-c\C-c"
    'c-includes-mode-goto-occurrence)
  (define-key c-includes-mode-map "\C-m"
    'c-includes-mode-goto-occurrence))

(defun c-includes-mode-mouse-goto (event)
  "In C Includes mode, go to the occurrence whose line you click on."
  (interactive "e")
  (let (buffer pos)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
        (c-includes-mode-goto-occurrence)))))

(defvar c-includes-buffer "*Includes*")

(defun c-includes-mode ()
  "Major mode for output from \\[c-includes].
\\<c-includes-mode-map>Move point to one of the items in this buffer,
then use \\[c-includes-mode-goto-occurrence] to go to the occurrence
that the item refers to.

\\{c-includes-mode-map}"
  (kill-all-local-variables)
  (use-local-map c-includes-mode-map)
  (setq major-mode 'c-includes-mode)
  (setq mode-name "C Includes")
  (run-hooks 'c-includes-mode-hook))

(defun c-includes-mode-goto-occurrence ()
  "Go to the occurrence the current line describes."
  (interactive)
  (beginning-of-line)
  (let ((count 0)
        (eol (save-excursion (end-of-line) (point)))
        line file)
    (cond ((re-search-forward "^\\s-*\\([0-9]+\\):" eol t)
           (setq line (string-to-number (match-string 1)))
           (while (and count
                       (re-search-backward "^=>\\s-*\\([-+]\\)\\(.*\\)$"
                                           nil t))
             (if (equal (match-string 1) "+")
                 (if (= count 0)
                     (progn
                       (push-mark)
                       (setq file (match-string-no-properties 2)
                             count nil)
                       (find-file-other-window file)
                       (goto-line line))
                   (setq count (1- count)))
               (setq count (1+ count)))))
          ((re-search-forward "^=>.*(line \\([0-9]+\\))" eol t)
           (push-mark)
           (goto-line (string-to-number (match-string 1)))))))


;;; c-includes.el ends here

