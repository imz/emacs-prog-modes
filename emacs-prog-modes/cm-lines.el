;; $Id: cm-lines.el,v 1.1 2001/05/22 03:23:33 burton Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
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

(defvar cm-lines-threshold 1000 "The maximum number of lines allowed in a file.")

(defvar cm-lines-explanation "It is generally a bad idea to have files which are too large.")

(defvar cm-lines-resolution "Try to break this file's functionality into multiple files.")

(defun cm-lines-warning()
  "Require that the current file have no more than
`cm-lines-threshold' lines."
  (interactive)

  (save-excursion 
    (if (< cm-lines-threshold (count-lines (point-min) (point-max)))

        (cm-report-warning (format "File has more than %i %s" cm-test-warning-max-line-count "lines.")
                           cm-lines-explanation
                           cm-lines-resolution))))

;;register default functions
(cm-register-warning 'cm-lines-warning)

(provide 'cm-lines)
