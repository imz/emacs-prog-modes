;;; project-mode-name.el --- Add project name to the mode-name variable via
;;; directory

;; $Id: project-mode-name.el,v 1.2 2001/07/23 11:00:53 burton Exp $

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
;; I wanted a way to show the "project" name on the mode line for my buffers.
;; Basically it changes the 'mode-name' variable to include the project.  This
;; way on buffers which I am editing which are mapped to projects, my modeline
;; will say.
;;
;; SGML:Reptile
;;
;; Where SGML is the major mode and the project name is Reptile.
;;
;;;
;; Install:
;;
;; Put the following in your .emacs file:
;;
;; (require 'project-mode-name)
;;
;; Then you need to set your main project directory:
;;
;; (setq pmn-project-directory "/projects")
;;
;; Then you can create explicit directory maps:
;;
;; (pmn-add-project-map "~" "Home")
;; (pmn-add-project-map "/projects/sierra" "Sierra")
;;
;; See the `pmn-add-project-map' function for more info.

;;; TODO:
;;
;; - See 'uniquify' for modifying the 'buffer-name'.  We need to also
;; incorporate the project name into the buffer name.
;;
;; - This package should be called 'project-name' and should set a new
;; buffer-local-variable named 'project-name' which could be used by other
;; buffers.
;;
;; - Completion tool for setting the filename based on directory and map.
;;
;; - Turn this into a minor mode which is installed via a hook and can be
;;   uninstalled.

;;; Code:
(defvar pmn-project-directory nil "The directory where you keep all your projects.")

(defvar pmn-project-map-alist nil "Associated list for mapping hard directories
to project names.  Do not modify this variable directly.  Instead use
`pmn-add-project-map'")

(defvar project-name nil "Name of the current project.")
(make-variable-buffer-local 'project-name)

(defun pmn-add-project-map(directory project)
  "Adds the given directory and project to `pmn-project-map-alist'.  A function
is used so that we can expand the filename."

  ;;expand the directory and clean it.
  (setq directory (pmn-clean-directory (expand-file-name directory)))
  
  (add-to-list 'pmn-project-map-alist (cons directory project)))
  

(defun pmn-guess-by-map(filename)
  "Uses `pmn-project-map-alist' to determine project name.  "

  (let(project test-project directory)

    (setq directory (expand-file-name (file-name-directory filename)))

    (while directory

      ;;trim the end of the directory
      ;;(setq directory (substring directory 0 (1- (length directory))))

      (setq test-project (cdr (assoc directory pmn-project-map-alist)))

      (if test-project
          (setq project test-project))
      
      (setq directory (pmn-get-parent-directory directory)))
    project))

(defun pmn-clean-directory(directory)
  "Make sure we have a trailing / on a directory name."

  (if (not (string-match "/$" directory))
      (setq directory (concat directory "/")))

  directory)

(defun pmn-guess-by-directory(filename)

  (if pmn-project-directory
      (progn

        ;;only run if pmn is configured.
        (setq pmn-project-directory (expand-file-name pmn-project-directory))

        ;;make sure we have a trailing /
        (setq pmn-project-directory (pmn-clean-directory pmn-project-directory))
        
        (let(regexp project)

          (setq regexp (concat "\\(" pmn-project-directory "\\)\\([^/]+\\)"))

          (if (string-match regexp filename)
              (progn

                (setq project (match-string 2 filename))

                project))))))
                
(defun pmn-find-file-hook()
  "Guess the project based on the filename."
  (interactive)

  (let(project filename)

    (setq filename (buffer-file-name))
    
    ;;try using a map
    (setq project (pmn-guess-by-map filename))

    ;;i this fails try by directory
    (if (null project)
        (setq project (pmn-guess-by-directory filename)))

    ;;NOTE: don't do anything stupid here like auto capitalization.
    ;;This might fail and we don't want to give the user a wrong project name.

    ;;ok.. now update the mode-name if we should

    (if (and project
             (not (equal project ""))
             (not (string-match ":" mode-name)))
        (setq mode-name (concat mode-name ":" project)))))

(defun pmn-get-parent-directory(directory)
  "Given a directory. Get it's parent or nil if we are given '/'."

  (if (string-equal directory "/")
      nil

    (if (string-match "\\(.+\\)/$" directory)
        ;;trim the trailing / if necessary
        (setq directory (match-string 1 directory)))
        
    (if (string-match "/\\([^/]+/\\)+" directory)
        (setq directory (match-string 0 directory))
      (setq directory "/"))

    directory))

(add-hook 'find-file-hooks 'pmn-find-file-hook)

(provide 'project-mode-name)
        
;;; project-mode-name.el ends here
