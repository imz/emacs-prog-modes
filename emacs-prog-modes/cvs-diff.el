;;; clipper.el --- save strings of data for further use.

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: clip save text
;; Version: 1.0.0

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

(require 'diff-mode)

(defun cvs-diff(filename)
  "cvs-diff just like function `vc-diff'."
  (interactive
   (list
    (read-file-name "File or directory: "
                    (buffer-file-name))))

  (if (file-directory-p filename)
      (cvs-diff--do-directory filename)
    (cvs-diff--do-file filename)))

(defun cvs-diff--do-file(path)
  "Do a CVS diff for a file."

  ;;it is acceptable here to change buffers and modify windows.
  
  (find-file path)
  (vc-diff nil))

(defun cvs-diff--do-directory(path)
  "Do a CVS diff for a directory."

  (message "Running 'cvs diff' on directory...")

  (setq path (expand-file-name path))
  
  (let((inhibit-read-only t)
       (buffer (get-buffer-create "*vc-diff*")))

    (set-buffer buffer)
    (erase-buffer)

    (setq default-directory path)

    (call-process "cvs" nil buffer t "diff" "-u")

    (goto-char (point-min))

    (cvs-diff--cleanup)
    
    (diff-mode)
    
    (display-buffer buffer))

  (message "Running 'cvs diff' on directory...done"))

(defun cvs-diff--cleanup()
  "Cleanup extra garbage from teh diff output."

  (let((inhibit-read-only t))
    (save-excursion

      (while (re-search-forward "^\\?.*$" nil t)
        (delete-region (match-beginning 0)
                       (1+ (match-end 0))))

    (save-excursion

      (while (re-search-forward "^cvs server: .*$" nil t)
        (delete-region (match-beginning 0)
                       (1+ (match-end 0))))))))
  
(provide 'cvs-diff)
