;;; jde-ant-multi-build.el --- allow multiple build targets for jde-ant-build

;; Author: Michael Schierl <schierlm@gmx.de>
;; Keywords: java, jde, ant
;; Version:
(defconst jde-ant-multi-build-version "1.0")

;; Copyright (C) 2003 Michael Schierl.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; this is an addon for `jde.el -- Integrated Development Environment
;; for Java.'  It adds support for building multiple ant targets at
;; once without losing support for completion; every target is shown
;; twice -- once with a `+' sign.  If you select a target with plus
;; sign, you can add another target to build.

;; For usage add this file to your load path and
;; (require 'jde-ant-multi-build)
;; to your .emacs file

;;; Code:
(eval-after-load "jde-ant"
  '(jde-ant-multi-build-do-replace))

(defun jde-ant-multi-build-do-replace ()
  "Replace the old `jde-ant-build' function."
  
  (fset 'jde-ant-build-old (symbol-function 'jde-ant-build))

  (defun jde-ant-build (buildfile target &optional interactive-args)
    "Build the current project using Ant."
    (interactive
     (let (buildfile history target target2 interactive-args)
       (setq buildfile (jde-ant-interactive-get-buildfile))
       (setq history (jde-ant-get-from-history buildfile))
       ;;get the target using completion.
       (when jde-ant-read-target
	 (setq target nil)
	 (while (or (not target) (string-match "\\+$" target))
	   (setq target2 (completing-read
			  (concat "Target to build"
				  (if target (concat " (" target "): ") ": "))
			  (jde-ant-add-plus (jde-ant-get-target-alist
					     buildfile))
			  nil
			  t
			  (if target nil (car history))
			  'history))
	   (if target
	       (setq target
		     (concat (substring target 0 (1- (length target)))
			     " " target2))
	     (setq target target2))))
       
       ;; Setting the history for future use
       (jde-ant-add-to-history buildfile history)
       
       (setq target (jde-ant-escape target))
       (if jde-ant-read-args
	   (setq interactive-args (read-from-minibuffer
				   "Additional build args: "
				   (nth 0 jde-ant-interactive-args-history)
				   nil nil
				   '(jde-ant-interactive-args-history . 1))))
       (setq jde-ant-interactive-buildfile buildfile)
       ;;return our new arguments.
       ;;This should be a list of buildfile, target and optional-args.
       (list buildfile target interactive-args)))
    (jde-ant-build-old buildfile target interactive-args)))
  
(defun jde-ant-add-plus (l)
  "Use the List L of targets and add each one with a `+' sign behind."
  (if l (cons (car l) (cons (list (concat (car (car l)) "+"))
			    (jde-ant-add-plus (cdr l))))
    nil))

(provide 'jde-ant-multi-build)

;;; jde-ant-multi-build.el ends here
