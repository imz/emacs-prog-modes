;;; googlens.el --- 

;; $Id: googlens.el,v 1.3 2003/01/10 05:57:59 burton Exp $

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

;; Lookup a query via the google I'm Feeling Lucky option and return it.
;; 

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; Code:

(defun googlens(query)
  "Lookup the given query on Google and return the URL it is running under."

  (save-excursion
    (let((buffer (get-buffer-create "*googlens*"))
         (process nil))

      (setq process (open-network-stream "googlens" buffer "www.google.com" 80))

      (process-send-string process
                           (concat "GET "
                                   "http://www.google.com/search?btnI=I'm+Feeling+Lucky&q="
                                   (replace-regexp-in-string " "
                                                             "+"
                                                             query)
                                   " HTTP/1.0\n\n"))

      ;;now terminate this process
      ;;(process-kill-without-query process)

      (process-send-eof process)

      (accept-process-output process)
      
      (set-buffer buffer)

      (goto-char (point-min))

      (assert (re-search-forward "^Location: \\(.*\\)\r" nil t)
              nil "Unable to find redirect")

      (let((result (match-string 1)))
      
        (kill-buffer buffer)

        result))))

(defun googlens-insert(query)
  "Insert the result of a Google query at point."
  (interactive
   (list
    (let((default nil))

      (when (looking-at "\w")
        (setq default (match-string 0)))

      (read-string "Query: " default))))

  (insert (googlens query)))

(defun googlens-link-region(query start end)
  "Create an HTML anchor out of the given region."
  (interactive
   (list
    (read-string "Query: "
                 (buffer-substring-no-properties (region-beginning)
                                                 (region-end)))

    (region-beginning)
    (region-end)))

   (save-restriction
     (narrow-to-region start end)

     (goto-char (point-min))
     (insert (format "<a href=\"%s\">" (googlens query)))
     (goto-char (point-max))
     (insert "</a>")))

(provide 'googlens)

;;; googlens.el ends here