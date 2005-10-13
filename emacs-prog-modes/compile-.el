;;; compile-.el --- Extensions to `compile.el'.
;;
;; Filename: compile-.el
;; Description: Extensions to `compile.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004, Drew Adams, all rights reserved.
;; Created: Tue Nov 16 17:04:11 2004
;; Version: 21.0
;; Last-Updated: Mon Oct 03 12:45:05 2005
;;           By: dradams
;;     Update #: 21
;; Keywords: tools, processes
;; Compatibility: GNU Emacs 21.x,GNU Emacs 22.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `compile.el'.
;;
;;  See also the companion file `compile+.el'.
;;        `compile-.el' should be loaded before `compile.el'.
;;        `compile+.el' should be loaded after `compile.el'.
;;
;;
;;  New user option (variable) defined here: `compile-regexp-face'.
;;
;;  Function `fit-1-window-frames-on' (defined in `fit-frame.el') is
;;  added here to `compilation-finish-functions'.
;;
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'compile+)
;;
;;  Library `compile-' requires these libraries:
;;
;;    `avoid', `fit-frame', `frame-cmds', `frame-fns', `misc-fns',
;;    `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2004/11/26 dadams
;;     Require frame-fns.el[c].
;; 2004/11/16 dadams
;;     New version for Emacs 21.  Old version renamed to compile-20.el.
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

(require 'fit-frame nil t) ;; (no error if not found): fit-frame
(require 'frame-fns nil t) ;; 1-window-frames-on

;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defvar compile-regexp-face
  (or (and (boundp 'skyblue-background-face) skyblue-background-face)
      (and (fboundp 'set-face-background)
           (fboundp 'x-color-defined-p)
           (x-color-defined-p "SkyBlue")
           (prog1 (make-face 'grep-regexp-face)
             (set-face-background 'grep-regexp-face "SkyBlue")))
      'highlight)
  "*Face for highlighting `compile' regexps.")


;;;;; ;;;###autoload
;;;;; (defvar compile-buffer-mouse-face 'underline
;;;;;   "*Face for highlighting mouse-overs in compilation buffer.")


(when (and (fboundp 'fit-frame) (fboundp '1-window-frames-on))
  (defun fit-1-window-frames-on (buf &optional ignored)
    "Resize buffer BUF's one-window frame(s) to fit the buffer.
Usable, e.g., as a member of `compilation-finish-functions'."
    ;; Optional arg IGNORED is ignored.
    ;; It is for compatibility with `compilation-finish-functions'.
    (let ((frs (1-window-frames-on buf)))
      (while frs
        (fit-frame (car frs))           ; Defined in `fit-frame.el'.
        (setq frs (cdr frs)))))
  (add-hook 'compilation-finish-functions 'fit-1-window-frames-on))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'compile-)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile-.el ends here
