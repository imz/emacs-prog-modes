;;; emake-frame.el --- Enhanced make-frame operation for Emacs

;;; $Id: emake-frame.el,v 1.7 2002/10/30 12:54:51 burton Exp $

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: frames new customization
;; Version: 2.0.1

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

;; This is a simple way to easily create new frames under Emacs.  It might not
;; work in your configuration, but I like it.

;; Basically it is setup like the bindings in Mozilla for creating new frames.
;; C-n creates the new frame which is a child of the current frame.  Note that
;; by default Emacs uses 'next-line' on C-n but in modern installations this is
;; redundant because most users use the arrow keys IMO.

;; I usually keep my main emacs window maximized.  If I want to go open another
;; frame but I currently have a sensitive window geometry (say while I am in
;; gnus) I will just create a new smaller frame with emake-frame-command.

;;; Installation

;; put this in your lisp load path and just do a (require 'emake-frame) in your
;; .emacs

;;; TODO:
;;
;; - include the ability to inherit the current doc by default.  and a prefix
;;   arg for enabling it with C-u C-n

;;; History
;;

;; Fri Jan 18 2002 12:46 PM (burton@openprivacy.org): added delete-some-frames
;; which deletes all frames except the current one.  This becomes a problem when
;; running Emacs for long periods of time because we end up with extra frames
;; that we don't need.  I had about 30 extra frames when I decided to write this
;; function.
;; 
;; Sat Dec 08 2001 12:07 PM (burton@openprivacy.org): : now using customization
;; for setup.

;; Fri Dec  1 05:40:45 2000 (burton): need to make sure that *scratch* isn't
;; read only.

;; dired required to setup key bindings there.
(require 'dired) 

(defcustom emake-frame-new-frame-width 130
  "Specify the width of the new frame.  See `make-frame' for more information."
  :group 'emake-frame)

(defcustom emake-frame-new-frame-height 75
  "Specify the height of the new frame.  See `make-frame' for more information."
  :group 'emake-frame)

(defcustom emake-frame-new-frame-left 50
  "Specify the left offset of the new frame.  See `make-frame' for more
information."
  :group 'emake-frame)

(defcustom emake-frame-new-frame-top 50
  "Specify the top offset of the new frame.  See `make-frame' for more
information."
  :group 'emake-frame)

(defcustom emake-frame-default-buffer "*scratch*"
  "The buffer to open when emake-frame-command is run."
  :group 'emake-frame)

(defun emake-frame()
  "Make a child frame to the current frame with the correct buffer.  When
complete return the frame created."

  (interactive)

  (let(frame)

    ;;FIXME: we can't provide frame params like this because it breaks a lot of
    ;;things.  Instead I should just modify default-frame-alist and provide that
    ;;info directly
    (select-frame (make-frame (list (cons 'left emake-frame-new-frame-left)
                                    (cons 'top emake-frame-new-frame-top)
                                    (cons 'width emake-frame-new-frame-width)
                                    (cons 'height emake-frame-new-frame-height))))
    ;;(select-frame frame))))

    (view-buffer emake-frame-default-buffer)

    (if (string= emake-frame-default-buffer "*scratch*")
        (toggle-read-only -1))))

(defun delete-some-frames()
  "Delete all frames except the current one."
  (interactive)
  
  (let((frame-list (frame-list)))

    (dolist(frame frame-list)
      
      (when (not (equal (selected-frame) frame))

        (delete-frame frame)))))

;; make a new window just like Mozilla.. this
(global-set-key [?\C-n] 'emake-frame)

;; kill the current frame
(global-set-key [?\C-x?\C-n] 'delete-frame)

;;make sure to setup the same binding in dired....
(define-key dired-mode-map [?\C-n] 'emake-frame)
(define-key dired-mode-map [?\C-x?\C-n] 'delete-frame)

(provide 'emake-frame)

;;; emake-frame.el ends here
