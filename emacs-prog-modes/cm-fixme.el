;; $Id: cm-fixme.el,v 1.1 2001/09/28 14:37:51 burton Exp $

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
;; Provides warnings if too many FIXME comments were found in your buffer.

(defvar cm-fixme-threshold 10 "Maximum number of FIXME comments that can present.")

(defvar cm-fixme-explanation "There are too many FIXME comments  in this file.")

(defvar cm-fixme-resolution "Try to minimize the number of FIXME comments..")

(defun cm-fixme-warning()
  "Warn on FIXME comments."

  (save-excursion
    (beginning-of-buffer)

    (let((case-fold-search nil)
         (count 0))

      (while (re-search-forward "FIXME" nil t)
        (setq count (1+ count)))

      (if (> count
             cm-fixme-threshold)

          (cm-report-warning (format "File has %i FIXME comments (maximum is %s)" count cm-fixme-threshold)
                           cm-fixme-explanation
                           cm-fixme-resolution)))))

(cm-register-warning 'cm-fixme-warning)
  
(provide 'cm-fixme)
