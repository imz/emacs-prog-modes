;;; cvs-keywords.el --- highlight cvs keywords

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: cvs highlight keywords
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

;; font-lock information which can highlight CVS keywords in the current buffer

;;; Install:

;; (require 'cvs-keywords) in your .emacs

;;; History:

;;; Code:
(defface cvs-keywords-face '((t (:italic t))) "Face used to show cvs keywords.")

(defun cvs-keywords-font-lock()
  "Adds a font-lock for buffers that user opens"
  (interactive)

  ;;font-lock which starts with a day... ends with a colon.
  (font-lock-add-keywords nil
                          '(("\\(\\$\\(Author\\|Date\\|Header\\|Id\\|Name\\|Locker\\|Log\\|RCSfile\\|Revision\\|Source\\|State\\):.+\\$\\)"
                             1 'cvs-keywords-face append))))
  
(add-hook 'font-lock-mode-hook 'cvs-keywords-font-lock)

(provide 'cvs-keywords)

;;; cvs-keywords.el ends here
