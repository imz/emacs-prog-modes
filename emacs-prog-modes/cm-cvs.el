;; $Id: cm-cvs.el,v 1.1 2001/05/22 03:23:33 burton Exp $

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
;;
;; CVS support for code-metrics, warns if you aren't using any CVS tags, etc.


;;HACK: had to concat two strings so that CVS wouldn't expand the Id string
(defvar cm-cvs-resolution 
  (concat "Insert the string '\$Id: "
          "$' somewhere in your buffer, possibly in a comment near the top of the file.")
  "Resolution string when CVS Id strings are not in the current buffer.")

(defvar cm-cvs-warning "No CVS \$Id tag."
  "Code Metric CVS warning message")
  
(defvar cm-cvs-explanation
  "CVS can rewrite your file to include version info.  In order to better manage your project this data should be included."
  "Code Metric CVS explanation")
  
(defun cm-cvs-id-warning()
  "Require that the current buffer have the CVS keyword ID if it is handled by
version control."
  (interactive)

  (if vc-mode
      (save-excursion
        (beginning-of-buffer)
        (if (not (search-forward "$Id: " nil t))

            (cm-report-warning cm-cvs-warning cm-cvs-explanation cm-cvs-resolution)))))

(cm-register-warning 'cm-cvs-id-warning)

(provide 'cm-cvs)
