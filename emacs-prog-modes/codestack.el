 ;;; codestack.el --- maintain a list of source bookmarks as a stack

;; $Id: codestack.el,v 1.7 2003/01/10 05:57:59 burton Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
;; Version: 1.0.1

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
   
;; Implements a stack of points (bookmarks) within a set of buffers and lets
;; you jump between them.
;; 
;;
;; Example of operation:
;;
;; http://www.peerfear.org/rss/permalink/2002/12/17/1040113843-CodeStack__Source_Bookmark_Stack_Management.shtml

;;; NOTES:

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; TODO:
;; 
;; - implement data persistence.
;;
;; - What are we going to do about marks in files that aren't open?  Maybe we
;; should have our own data format.
;;
;; '(file . DATA)
;;  (marker . DATA)
;;
;;  marker can be nill at which point we can have a 'line-number

;;; Code:

(require 'semantic)
(require 'blinkline)

(defface codestack-entry-face '((t (:background "darkslategray")))
  "Face used for a codestack entry."
  :group 'codestack)

(defface codestack-blinkline-face '((t (:inherit blinkline-face)))
  "Face used for a blinking a line."
  :group 'codestack)

(defvar codestack--stack nil "Stack of markers for all buffers and points.")

(defvar codestack-buffer-name " *codestack*" "Buffer name to use for stack browsing.")

(defvar codestack-browse-layout '(("" . 3)
                                  ("Buffer" . 20)
                                  ("Line" . 7)
                                  ("Function" . 25)
                                  ("Content" . nil))
  "Layout to use for the browse buffer")

(defvar codestack-mode-map (make-sparse-keymap)
  "Keymap used in Codestack config mode buffers")

(defvar codestack-mode-syntax-table nil
  "Codestack config mode syntax table")

(defvar codestack-mode-hook nil
  "*List of hook functions run by `codestack-mode' (see `run-hooks')")

(defcustom codestack-delete-window-after-select nil
  "Delete the codestack window after we select a stack entry."
  :type 'boolean
  :group 'codestack)

(defcustom codestack-browse-after-push t
  "Browse the codestack after we push an entry on the stack.."
  :type 'boolean
  :group 'codestack)

(defimage codestack-entry-image ((:type xpm :ascent center :data "/* XPM */
static char * sb_tag+_xpm[] = {
\"20 15 4 1\",
\" 	c None\",
\".	c #828282\",
\"+	c #000000\",
\"@	c #FFF993\",
\"                    \",
\"                    \",
\"    ............... \",
\"   .+++++++++++++++.\",
\"  .+@@@@@@@@@@@@@@+.\",
\" .+@@@@@@++@@@@@@@+.\",
\".+@@@@@@@++@@@@@@@+.\",
\".+@++@@++++++@@@@@+.\",
\".+@++@@++++++@@@@@+.\",
\".+@@@@@@@++@@@@@@@+.\",
\" .+@@@@@@++@@@@@@@+.\",
\"  .+@@@@@@@@@@@@@@+.\",
\".  .+++++++++++++++.\",
\"    ............... \",
\"                    \"};"))
  "Image used for the display of a codestack entry.")

(defun codestack-push()
  "Push the current line in the current file into the stack."
  (interactive)
  
  ;;make sure we don't have a marker within the same line.. that would be bad
  ;;(and stupid).

  (let((marker (make-marker)))

    (set-marker marker (point-at-eol))

    (add-to-list 'codestack--stack marker)
    (codestack-create-overlay marker))

  (when (or codestack-browse-after-push
             (get-buffer-window codestack-buffer-name))
    (codestack-browse)))

(defun codestack-kill()
  "Kill a stack from the frame we are in."
  (interactive)

  (let((marker (get-text-property (point) 'codestack-marker)))
    (if (and (equal major-mode 'codestack-mode)
               marker)
        (progn
          (setq codestack--stack (delete marker codestack--stack))
          (codestack-kill-overlays marker)
          (codestack-browse))
      ;;else try to kill it from the current buffer
      (let*((overlays-at (overlays-at (point))))

        (dolist(overlay overlays-at)
          (when(equal (overlay-get overlay 'face)
                      'codestack-entry-face)

            (setq marker (overlay-get overlay 'codestack-marker))
            
            (setq codestack--stack (delete marker
                                           codestack--stack))
            (codestack-kill-overlays marker)))))))

(defun codestack-browse()
  "Display the stack in a window so that we can navigate through the stack."
  (interactive)

  ;;go through each marker displaying the buffer name, line number, and text.

  (save-excursion
    (let((index 0)
         (inhibit-read-only t)
         (marker-function nil)
         (marker-content nil)
         (marker-line-number nil))
      
      (set-buffer (get-buffer-create codestack-buffer-name))
      (erase-buffer)
      (setq left-margin-width 4)
      
      (dolist(entry codestack-browse-layout)
        (let((column-name (car entry)))

          (add-text-properties 0 (length column-name)
                               '(face font-lock-keyword-face)
                               column-name)
          
          (codestack-browse--insert-cell column-name (cdr entry))))

      (insert "\n")
      
      (dolist(entry codestack-browse-layout)

        (codestack-browse--insert-cell (make-string (length (car entry))
                                                    ?\-)
                                       (cdr entry)))

      (insert "\n")
      
      (dolist(marker codestack--stack)

        (if (or (null (marker-buffer marker))
                (not (get-buffer (marker-buffer marker))))
            ;;this is an invalid marker... remove it.
            ;;NOTE: maybe we should support buffers that were deleted?
            (setq codestack--stack (delete marker codestack--stack))

          (insert-image codestack-entry-image nil 'left-margin)

          (codestack-browse--insert-cell (number-to-string index)
                                         (cdr (nth 0 codestack-browse-layout)))

          (codestack-browse--insert-cell (buffer-name (marker-buffer marker))
                                         (cdr (nth 1 codestack-browse-layout)))

          (save-excursion
            (set-buffer (marker-buffer marker))

            (goto-char (marker-position marker))

            (setq marker-function (codestack-get-function))
          
            ;;get the line number
            (setq marker-line-number (count-lines (point-min) (point)))
          
            ;;get the marker content of this line
            (setq marker-content
                  (codestack-string-trim
                   (buffer-substring (point-at-bol) (point-at-eol)))))

          (codestack-browse--insert-cell (number-to-string marker-line-number)
                                         (cdr (nth 2 codestack-browse-layout)))

          (codestack-browse--insert-cell marker-function
                                         (cdr (nth 3 codestack-browse-layout)))

          (insert marker-content)

          ;;add a property for this marker
          (add-text-properties (point-at-bol) (point-at-eol)
                               (list 'codestack-marker marker))

          (insert "\n")
        
          (setq index (1+ index))))
      (insert "\n")
      
      (goto-char (point-min))))

  (let((window (get-buffer-window codestack-buffer-name)))

    (if window
        (select-window window)
      
      (split-window)
      (other-window 1)
      (switch-to-buffer codestack-buffer-name)
      (codestack-mode)
      (shrink-window-if-larger-than-buffer)
      (enlarge-window 2)))

  (codestack-bos))

(defun codestack-bos()
  "Goto the beginning of the stack (bos)."

  (set-buffer codestack-buffer-name)
  (goto-char (point-min))
  (forward-line 2))

(defsubst codestack-string-trim (string)
  "Lose leading and trailing whitespace.  Also remove all properties
from string."
  (if (string-match "\\`[ \t\n]+" string)
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t\n]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  string)

(defun codestack-browse--goto-marker()
  "Goto the marker on the current line (assuming we have one)"
  (interactive)
  
  (let((marker (get-text-property (point) 'codestack-marker)))

    (when marker

      (let((other-window nil))
        (save-window-excursion
          (other-window -1)
          (setq other-window (selected-window)))
      
        (set-window-buffer other-window (marker-buffer marker)))

      (save-excursion

        (set-buffer (marker-buffer marker))

        (goto-char (marker-position marker))
      
        ;;FIXME: we can't do this
        (set-window-point (get-buffer-window (marker-buffer marker))
                          (marker-position marker))

        (recenter)
        ;;blink the current line
        (blinkline))

      (when codestack-delete-window-after-select
        (codestack-kill-buffer)))))
  
(defun codestack-browse--insert-cell(value length)
  "Insert a length fixed cell into the browse buffer."

  (if (or value
          (> (length value) 0))
      (progn
        (insert value)

        (if (and length
                 (> length (length value)))
            (insert (make-string (- length (length value)) ?\ ))
          (insert " ")))
    (insert (make-string length ?\ ))))

(defun codestack-mode ()
  "Codestack mode."
  (interactive)
  (kill-all-local-variables)
  (use-local-map codestack-mode-map)
  ;;(set-syntax-table codestack-mode-syntax-table)

  (setq mode-name "codestack")
  (setq major-mode 'codestack-mode)

  (setq truncate-lines t
        buffer-read-only t)
  
  (run-hooks 'codestack-mode-hook))

(defun codestack-get-function()
  "Get the function that we are in."
  (interactive)

  (save-excursion
    (let((nonterminal nil)
         (function-name nil))

      (semantic-bovinate-toplevel) 

      (while (and (null function-name)
                  (setq nonterminal (semantic-find-nonterminal-by-overlay-prev)))

        (goto-char (semantic-token-start nonterminal))
        
        (when (equal 'function (semantic-token-token nonterminal))
          (setq function-name (semantic-token-name nonterminal))))

      function-name)))

(defun codestack-create-overlay(marker)
  "Create an overlay around the given marker."
  (interactive
   (list
    (let((marker (make-marker)))
      (set-marker marker (point))
      marker)))

  (save-excursion
    (set-buffer (marker-buffer marker))
    (goto-char (marker-position marker))

    (let*((overlay (make-overlay 0 0))
          (string "x"))

      ;;which one do we have to set???
      (setq left-margin-width 3)
      (set-window-margins (selected-window) 3)
      
      (overlay-put overlay 'priority 5)
      (overlay-put overlay 'face 'codestack-entry-face)
      (overlay-put overlay 'modification-hooks '(codestack-create-overlay--modification-hook))
      (overlay-put overlay 'codestack-marker marker)
      (overlay-put overlay 'evaporate t)

      ;;setup the required image options
      (put-text-property 0 (length string) 'display
                           (list (list 'margin 'left-margin)
                                 codestack-entry-image)
                         string)

      (overlay-put overlay 'put-image t)
      (overlay-put overlay 'before-string string)
      ;;codestack-entry-image
      
      (move-overlay overlay (point-at-bol) (1+ (point-at-eol))))))

(defun codestack-create-overlay--modification-hook(&optional overlay pafter pbegin pend length)
  "Modify the overlay so that we keep it on the current line."

  (save-excursion
    (let*((overlays-at (overlays-at (point))))

      (dolist(overlay overlays-at)

        (when(equal (overlay-get overlay 'face)
                     'codestack-entry-face)

          (let*((marker (overlay-get overlay 'codestack-marker)))

            (save-excursion
              (goto-char (marker-position marker))
              
              (move-overlay overlay (point-at-bol) (1+ (point-at-eol))))))))))

(defun codestack-kill-overlays(marker)
  "Kill all codestack overlays at the given marker."

  (save-excursion
    (set-buffer (marker-buffer marker))
    
    (let*((overlays-at (overlays-at (1- (marker-position marker)))))

      (dolist(overlay overlays-at)

        (when(equal (overlay-get overlay 'face)
                     'codestack-entry-face)
          (delete-overlay overlay))))))

(defun codestack-toggle()
  "Toggle a codestack entry on the current line."
  (interactive)
  
  (let*((found nil)
        (overlays-at (overlays-at (point))))

      (dolist(overlay overlays-at)
        (when(equal (overlay-get overlay 'face)
                     'codestack-entry-face)
            (setq found t)))

      (if found
          (codestack-kill)
        (codestack-push))))

(defun codestack-kill-buffer()
  "Kill the codestack buffer."
  (interactive)

  (delete-window (get-buffer-window codestack-buffer-name)))

(define-key codestack-mode-map  [return] 'codestack-browse--goto-marker)
(define-key codestack-mode-map  "k" 'codestack-kill)
(define-key codestack-mode-map "\C-b" 'codestack-kill-buffer)

(provide 'codestack)

;;; codestack.el ends here