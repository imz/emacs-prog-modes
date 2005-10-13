;;; compile+.el --- Extensions to `compile.el'.
;;
;; Filename: compile+.el
;; Description: Extensions to `compile.el'.
;; Author: Drew ADAMS
;; Maintainer: Drew ADAMS
;; Copyright (C) 2004, Drew Adams, all rights reserved.
;; Created: Tue Nov 16 16:38:23 2004
;; Version: 21.0
;; Last-Updated: Mon Oct 03 12:45:52 2005
;;           By: dradams
;;     Update #: 637
;; Keywords: tools, processes
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `compile.el'.
;;
;;  See also the companion file `compile-.el'.
;;        `compile-.el' should be loaded before `compile.el'.
;;        `compile+.el' should be loaded after `compile.el'.
;;
;;
;;  ***** NOTE: The following function defined in `compile.el'
;;              has been REDEFINED HERE:
;;
;;  `compilation-goto-locus' - Use `compile-regexp-face'. Do not erase
;;                             highlighting. Display line number.
;;
;; Some bindings that would try to modify a compilation mode buffer
;; are unbound. Their key sequences will then appear to the user
;; as available for local (Compilation Mode) definition. That is,
;; we do this here: `(undefine-killer-commands
;;                       compilation-mode-map
;;                       (current-global-map))'
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'compile+)
;;
;;  Library `compile+' requires these libraries:
;;
;;    `avoid', `compile', `compile-', `fit-frame', `frame-cmds',
;;    `frame-fns', `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2004/11/16 dadams
;;     New version for Emacs 21. Old version renamed to compile+20.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'compile-) ;; compile-regexp-face
(require 'compile)

(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands
(require 'strings nil t) ;; (no error if not found): display-in-minibuffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Wipe out existing overlay.
(when (boundp 'compilation-highlight-overlay)
  (when (overlayp compilation-highlight-overlay)
    (delete-overlay compilation-highlight-overlay))
  (setq compilation-highlight-overlay nil))


;;; Undefine some bindings that would try to modify a Compilation mode buffer.
;;; Their key sequences will then appear to the user as available for
;;; local (Compilation Mode) definition.
(when (fboundp 'undefine-killer-commands)
  (undefine-killer-commands compilation-mode-map (current-global-map)))



;; REPLACES ORIGINAL in `compile.el':
;;
;; 1. Use `compile-regexp-face' instead of hard-coded region face.
;; 2. Do not erase highlighting, so not just temporary.
;; 3. Displays line number.
(defun compilation-goto-locus (msg mk end-mk)
  "Jump to an error corresponding to MSG at MK.
All arguments are markers.  If END-MK is non nil, mark is set there."
  (if (eq (window-buffer (selected-window))
	  (marker-buffer msg))
      ;; If the compilation buffer window is selected,
      ;; keep the compilation buffer in this window;
      ;; display the source in another window.
      (let ((pop-up-windows t))
	(pop-to-buffer (marker-buffer mk)))
    (if (window-dedicated-p (selected-window))
	(pop-to-buffer (marker-buffer mk))
      (switch-to-buffer (marker-buffer mk))))
  ;; If narrowing gets in the way of going to the right place, widen.
  (unless (eq (goto-char mk) (point))
    (widen)
    (goto-char mk))
  (if end-mk
      (push-mark end-mk nil t)
    (if mark-active (setq mark-active)))
  ;; If hideshow got in the way of
  ;; seeing the right place, open permanently.
  (dolist (ov (overlays-at (point)))
    (when (eq 'hs (overlay-get ov 'invisible))
      (delete-overlay ov)
      (goto-char mk)))

  ;; Show compilation buffer in other window, scrolled to this error.
  (let* ((pop-up-windows t)
	 ;; Use an existing window if it is in a visible frame.
	 (w (or (get-buffer-window (marker-buffer msg) 'visible)
		;; Pop up a window.
		(display-buffer (marker-buffer msg))))
	 (highlight-regexp (with-current-buffer (marker-buffer msg)
			     ;; also do this while we change buffer
			     (compilation-set-window w msg)
			     compilation-highlight-regexp)))
    (compilation-set-window-height w)

    (when (and highlight-regexp
	       (not (and end-mk transient-mark-mode)))
      (unless compilation-highlight-overlay
	(setq compilation-highlight-overlay
	      (make-overlay (point-min) (point-min)))
	(overlay-put compilation-highlight-overlay 'face compile-regexp-face))
      (with-current-buffer (marker-buffer mk)
	(save-excursion
	  (end-of-line)
	  (let ((end (point)))
	    (beginning-of-line)
	    (if (and (stringp highlight-regexp)
		     (re-search-forward highlight-regexp end t))
		(progn
		  (goto-char (match-beginning 0))
		  (move-overlay compilation-highlight-overlay (match-beginning 0) (match-end 0)))
	      (move-overlay compilation-highlight-overlay (point) end))
            ;; $$$$$ SHOULD SOMEHOW delete overlay on next user action
	    ;;;;;(sit-for 0.5)
	    ;;;;;(delete-overlay compilation-highlight-overlay)
            )))
      ;; Message about current line number and how to remove highlighting.
      ;; $$$$$ NEED TO CHANGE MESSAGE ABOUT HOW TO REMOVE HIGHLIGHTING TO WORK WITH OVERLAY.
      (if (fboundp 'display-in-minibuffer)
        (display-in-minibuffer 'event "Line "
                               (list blue-foreground-face (format "%s" (current-line)))
                               ". Use `"
                               (list blue-foreground-face
                                     (substitute-command-keys
                                      "\\[negative-argument] \\[highlight]"))
                               "' to remove highlighting (in a region).")
        (message (format "Line %s. %s" (current-line)
                         (substitute-command-keys
                          "`\\[negative-argument] \
\\[highlight]' to remove highlighting (in a region)."))))
      )))

;;;;;;;;;;;;;;;;;;

(provide 'compile+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile+.el ends here
