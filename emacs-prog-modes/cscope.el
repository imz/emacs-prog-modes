;;; cscope.el --- enhanced Emacs support for the cscope source browser

;; Copyright (C) 2003 Bryan O'Sullivan

;; Author: Bryan O'Sullivan <bos@serpentine.com>

;; $Id: s.cscope.el 1.16 03/08/29 12:54:28-07:00 bos@serpentine.com $

;; cscope.el ("this file") is free software; you can redistribute it
;; and/or modify it under the terms of version 2 of the GNU General
;; Public License ("the GPL") as published by the Free Software
;; Foundation.  You may not redistribute it or modify this file under
;; the terms of any other license, including any other version of the
;; GPL.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GPL
;; for more details.

;; You should have received a copy of the GPL along with this file,
;; GNU Emacs, or XEmacs; see the file COPYING (`C-h C-l').  If not,
;; write to the Free Software Foundation, Inc., 59 Temple Place -
;; Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode adds support for the cscope source browser into GNU Emacs
;; and XEmacs.

;; To get going as quickly as possible, load thie file into Emacs and
;; type `C-c c h'; this runs cscope-help-overview, which prints a
;; helpful usage overview.

;; There already exist two similar packages for Emacs: xcscope.el, by
;; Darryl Okahata (distributed with cscope itself), and a much older
;; cscope.el, which seems to have been lost to the mists of time.

;; Although this package takes some cues from Darryl's xcscope.el, I
;; find that package to be bafflingly complex, and overly tied to the
;; magic indexing script that it comes with.  This package shares no
;; code with xcscope.el, and is hopefully both easier to use and less
;; tied to an external program.

;; This code has been developed under XEmacs 21.4, and probably will
;; not work as well under GNU Emacs (albeit tested under 21.2).
;; Patches to enhance the portability of this code, fix bugs, and add
;; features are most welcome.  You can obtain a BitKeeper repository
;; for this package by cloning bk://bk-emacs.bkbits.net/cscope-fu

;; Please send problem reports and suggestions to bos@serpentine.com.

;;; Code:

(require 'thingatpt)

;; XEmacs has view-less, while GNU Emacs has view.  Joy.

(condition-case nil
    (require 'view-less)
  (error nil))
(condition-case nil
    (require 'view)
  (error nil))

(defgroup cscope nil
  "Cscope Source Browser."
  :group 'tools)

(defconst cscope-running-xemacs (string-match "XEmacs" emacs-version)
  "Is cscope.el running under XEmacs?")

(defcustom cscope-last-directory nil
  "The last directory that we searched containing a cscope database."
  :type 'directory
  :group 'cscope)

(defconst cscope-search-descriptions
  '(("symbol" . 0)
    ("global definition" . 1)
    ("functions called by" . 2)
    ("text string" . 3)
    (nil . 4) ;; change text string
    ("regular expression" . 5)
    ("file" . 6)
    ("files #including" . 7))
  "Descriptions of cscope search fields.

Cscope has a particularly bizarre way of specifying which kind of
search to perform.  Its curses user interface provides a menu of eight
searches.  When using the batch-oriented interface, you specify which
search to perform by providing its numeric offset into this menu.

This is vaguely hinted at in the cscope manual page, but in such a way
that you could only hope to understand it if you already knew how the
mechanism worked.   Aaargh.")

(defcustom cscope-binary "cscope"
  "The path to the cscope executable."
  :type '(file :must-match t)
  :group 'cscope)

(defcustom cscope-args nil
  "List of extra arguments to pass to cscope."
  :type '(string)
  :group 'cscope)

(defcustom cscope-special-tree-alist
  '(("Linux kernel" "Documentation/ramdisk.txt")
    ("GNU C library" "manual/libm-err.texi")
    ("klibc" "klibc/sleep.c")
    ("Pro64 compiler" "gccfe/gnu/f/lang-options.h"))
  "Alist of (TREE . FILES) pairs identifying special trees.
A special tree is a kernel or C library tree, for which cscope should
not additionally search for files in /usr/include.

TREE is the descriptive name of a tree\; FILES is a list of relative
paths to unique files within that tree.  If any of FILES exists, we
believe ourselves to be inside TREE."
  :type '(repeat (string . string))
  :group 'cscope)

(global-set-key [(meta ?\`)] 'cscope-search)
(global-set-key [(meta ?\])] 'cscope-next-result)
(global-set-key [(meta ?\[)] 'cscope-previous-result)

(global-set-key [(control c) c h] 'cscope-help-overview)

(defvar cscope-result-mode-map (make-sparse-keymap "cscope-result-mode-map")
  "Keymap for cscope-result-mode.")
(fset 'cscope-result-mode-map cscope-result-mode-map)

(define-key cscope-result-mode-map
  (if cscope-running-xemacs [button2] [mouse-2])
  'cscope-mouse-clicked)

(define-key cscope-result-mode-map [space] 'scroll-up-command)
(define-key cscope-result-mode-map [backspace] 'scroll-down-command)
(define-key cscope-result-mode-map [return] 'cscope-result-at-point)
(define-key cscope-result-mode-map [h] 'describe-mode)
(define-key cscope-result-mode-map [n] 'cscope-next-result)
(define-key cscope-result-mode-map [p] 'cscope-previous-result)
(define-key cscope-result-mode-map [Q] 'cscope-quit)
(define-key cscope-result-mode-map [q] 'cscope-kill)

(defvar cscope-last-buffer nil
  "The last buffer in which a cscope result was produced.")

(defvar cscope-last-point nil
  "The last point from which a cscope result was jumped to.")

(defvar cscope-result-mode-hook nil
  "*Hook called when entering cscope-result-mode.")

(defun cscope-result-mode ()
  "Major mode for displaying the results of cscope searches.

Middle mouse button or \\[cscope-result-at-point] on a result selects
a result.

Commands:
\\[cscope-next-result]		cscope-next-result
\\[cscope-previous-result]		cscope-previous-result
\\[cscope-result-at-point]		cscope-result-at-point
\\[scroll-up-command]		scroll-up-command
\\[scroll-down-command]	scroll-down-command
\\[cscope-quit]		cscope-quit
\\[cscope-kill]		cscope-kill
\\[describe-mode]		describe-mode
"
  (setq cscope-last-buffer (current-buffer)
	cscope-last-point 1
	major-mode 'cscope-result-mode
	mode-name "Cscope")
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (toggle-read-only t)
  (use-local-map cscope-result-mode-map)
  (setq truncate-lines t)
  (run-hooks 'cscope-result-mode-hook))

(unless (fboundp 'view-minor-mode)
  (defun view-minor-mode (prev-buffer exit-func)
    (view-mode)))

(defun cscope-exit-view-mode (buf)
  "Exit from view-mode.
We delete the current window if entering bk-view-mode split the
current frame."
  (when (and (eq buf (current-buffer))
	     (> (length (window-list)) 1))
    (delete-window))
  (when (buffer-live-p buf)
    (kill-buffer buf)))

(defun cscope-help-overview ()
  "This is an overview of Emacs support for the cscope source browser.

You are using the following version of the cscope.el package: $Revision: 1.16 $

You can find the source code, license (GPL v2), and credits for this
code by typing `M-x find-library cscope RET'.

Global key bindings:

Perform a search			\\[cscope-search]	cscope-search
Go to the next search result		\\[cscope-next-result]	cscope-next-result
Go to the previous search result	\\[cscope-previous-result]	cscope-previous-result"
  (interactive)
  (let ((prev-buf (current-buffer)) 
	(buf-name "*Cscope: Help Overview*"))
    (kill-buffer (get-buffer-create buf-name))
    (pop-to-buffer (get-buffer-create buf-name))
    (insert (documentation 'cscope-help-overview))
    (view-minor-mode prev-buf 'cscope-exit-view-mode)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (toggle-read-only t)
    (view-minor-mode prev-buf 'cscope-exit-view-mode)))
  
(defun cscope-quit (&optional kill)
  "Quit the current cscope search result buffer.
If KILL is non-kill, kill the buffer; otherwise, bury it."
  (interactive "P")
  (unless (eq major-mode 'cscope-result-mode)
    (error "Called outside a cscope-result-mode buffer!"))
  (let ((buf (current-buffer)))
    (when (> (length (window-list)) 1)
      (delete-window))
    (if kill
	(kill-buffer buf)
      (set-buffer buf)
      (bury-buffer))))

(defun cscope-kill ()
  "Kill the current cscope search result buffer."
  (interactive)
  (cscope-quit t))

(defun cscope-in-special-tree (&optional path)
  "Indicate whether PATH is inside a special tree.
See the variable `cscope-special-tree-alist' for what constitutes a
special tree."
  (interactive)
  (unless path
    (setq path (if (and (interactive-p) current-prefix-arg)
		   (read-file-name "Path name: ")
		 "")))
  (setq path (expand-file-name path))
  (let ((cur path)
	tree root)
    (while (and (not root) (> (length cur) 0))
      (let ((trees cscope-special-tree-alist))
	(while (and (not root) trees)
	  (when (some (function (lambda (file)
				  (file-exists-p (concat cur "/" file))))
		      (cdar trees))
	    (setq root cur
		  tree (caar trees)))
	  (setq trees (cdr trees)))
	(setq cur (let ((dir (file-name-directory cur)))
		    (substring dir 0 (1- (length dir)))))))
    (if (interactive-p)
	(if root
	    (message "The top of this %s tree is `%s'." tree root)
	  (message "This path is not inside a special tree!"))
      root)))

(defun cscope-find-cscope-files (&optional path)
  "Find cscope files up the tree from PATH."
  (interactive)
  (unless path
    (setq path (if (and (interactive-p) current-prefix-arg)
		   (read-file-name "Path name: ")
		 "")))
  (setq path (expand-file-name path))
  (let ((cur path)
	root)
    (while (and (not root) (> (length cur) 0))
      (cond
       ((file-exists-p (concat cur "/cscope.out"))
	(setq root cur))
       ((file-exists-p (concat cur "/.cscope/cscope.out"))
	(setq root (concat cur "/.cscope"))))
      (setq cur (let ((dir (file-name-directory cur)))
		  (substring dir 0 (1- (length dir))))))
    (if (interactive-p)
	(if root
	    (message "Found cscope files in `%s'." root)
	  (message "This path contains no cscope files!"))
      root)))

(defun cs-set-extent-property (start end property value)
  "Set a property on an extent or overlay, depending on your religion."
  (if cscope-running-xemacs
      (set-extent-property (make-extent start end)
			   property value)
    (overlay-put (make-overlay start end)
		 property value)))

(defun cscope-create-magic-link (style start end file line)
  "Create a magic mousable link to a chunk of text."
  (when style
    (cs-set-extent-property start end 'face style))
  (cs-set-extent-property start end 'mouse-face 'highlight)
  (cs-set-extent-property start end 'file+line (cons file line)))

(defun cscope-result-at-point (pnt &optional this-window save)
  "Jump to the cscope result at point PNT.
If THIS-WINDOW is non-nil, jump in this window, else in another."
  (interactive "d")
  (unless pnt
    (error "What is the point?"))
  (let ((prop (get-char-property pnt 'file+line)))
    (when prop
      (when (or save (interactive-p))
	(setq cscope-last-point pnt))
      (let ((type cscope-type)
	    (pattern cscope-pattern)
	    (file (car prop))
	    (line (cdr prop)))
	(if this-window
	    (find-file file)
	  (find-file-other-window file))
	(when line
	  (goto-line line)
	  (let ((end (save-excursion
		       (end-of-line)
		       (point))))
	    (cond
	     ((memq type '(0 1 3 7))
	      (if (search-forward pattern end t)
		  (goto-char (match-beginning 0))
		(beginning-of-line)))
	     ((eq type 5)
	      (if (re-search-forward pattern end t)
		  (goto-char (match-beginning 0))
		(beginning-of-line)))
	     (t
	      (beginning-of-line)))))))))

(defun cscope-mouse-clicked (event)
  "Translate the mouse clicks in a cscope result buffer to character events.
These are then handed off to `cscope-result-at-point'.

Handle frickin' frackin' gratuitous event-related incompatibilities."
  (interactive "e")
  (let ((pnt (if cscope-running-xemacs
		 (progn
		   (select-window (event-window event))
		   (event-point event))
	       (select-window (posn-window (event-end event)))
	       (posn-point (event-start event)))))
    (goto-char pnt)
    (setq cscope-last-point pnt)
    (cscope-result-at-point pnt nil t)))

(defun cscope-next-result (&optional count save)
  "Visit next cscope search result and corresponding source code.
A prefix arg specifies how many results to move\; negative means move
back to previous results."
  (interactive "p")
  (when (= count 0)
    (error "count cannot be zero!"))
  (unless (buffer-live-p cscope-last-buffer)
    (error "No cscope results to move through!"))
  (let ((this-window (not (eq (current-buffer) cscope-last-buffer)))
	(incr (if (> count 0) 1 -1)))
    (setq count (abs count))
    (set-buffer cscope-last-buffer)
    (goto-char cscope-last-point)
    (let (there)
      (while (not there)
	(when (> count 0)
	  (setq count (1- count)))
	(let ((prev (line-number)))
	  (forward-line incr)
	  (when (= prev (line-number))
	    (error "No more matches for %s" cscope-desc))
	  (when (and (= count 0)
		     (let ((prop (get-char-property (point) 'file+line)))
		       (and prop (cdr prop))))
	    (setq there (point)))))
      (cscope-result-at-point there this-window (or save (interactive-p))))))

(defun cscope-previous-result (&optional count save)
  "Visit previous cscope search result and corresponding source code.
A prefix arg specifies how many results to move\; negative means move
forward to next results."
  (interactive "p")
  (cscope-next-result (- count) (or save (interactive-p))))

(defun cscope-process-sentinel (process event)
  (let ((buf (process-buffer process))
	lines)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (let (whaa)
	(setq lines (if (looking-at "^cscope: \\(.*\\)$")
			(progn
			  (setq whaa (match-string 1))
			  0)
		    (1- (line-number (point-max)))))
	(if whaa
	    (message "Searching for %s ... %s!" cscope-desc whaa)
	  (message "Searching for %s ... %d match%s"
		   cscope-desc
		   lines
		   (if (= lines 1) "" "es")))))
    (if (= lines 0)
	(kill-buffer buf)
      (pop-to-buffer buf)
      (goto-char (point-min))
      (setq cscope-last-directory cscope-dir)
      (insert (format "%d match%s for %s:\n"
		      lines
		      (if (= lines 1) "" "es")
		      cscope-desc))
      (let (last)
	(while (looking-at "^\\([^ ]+\\) \\([^ ]*\\) \\([^ ]+\\) \\(.*\\)$")
	  (let* ((file-match 1)
		 (func-match 2)
		 (line-match 3)
		 (snip-match 4)
		 (file (buffer-substring (match-beginning file-match)
					 (match-end file-match)))
		 (func (buffer-substring (match-beginning func-match)
					 (match-end func-match)))
		 (line (buffer-substring (match-beginning line-match)
					 (match-end line-match)))
		 (snip (buffer-substring (match-beginning snip-match)
					 (match-end snip-match))))
	    (if (string= file last)
		(progn
		  (cscope-create-magic-link nil
					    (match-beginning 0)
					    (match-end 0)
					    file
					    (string-to-number line))
		  (delete-region (match-beginning file-match)
				 (1+ (match-end file-match))))
	      (cscope-create-magic-link 'bold
					(match-beginning file-match)
					(match-end file-match)
					file
					nil)
	      (cscope-create-magic-link nil
					(match-beginning func-match)
					(match-end 0)
					file
					(string-to-number line))
	      (setq last file)
	      (goto-char (match-beginning 0))
	      (insert "\n")
	      (goto-char (1+ (match-end file-match)))
	      (insert ":\n")
	      (delete-char 1))
	    (when (string= func "")
	      (insert "<global>"))
	    (search-forward " " nil t)
	    (insert "- ")
	    (search-forward " " nil t)
	    (let ((goal (- 32 (current-column))))
	      (while (> goal 0)
		(insert " ")
		(setq goal (1- goal)))))
	  (forward-line 1)
	  (beginning-of-line)))
      (cscope-result-mode))))

(defvar cscope-search-type-history nil
  "History of types of cscope searches performed.")
(defvar cscope-search-pattern-history nil
  "History of cscope search patterns.")

(defun cscope-search (type pattern)
  "Perform a cscope search of type TYPE for the string PATTERN.
If called interactively, this command prompts for both TYPE and
PATTERN.

For programmatic callers, who will surely need a description of the
truly weird meaning of TYPE, see the variable
`cscope-search-descriptions'."
  (interactive
   (let* ((def-type (if cscope-search-type-history
			(car cscope-search-type-history)
		      "symbol"))
	  (type (completing-read (format "Type of search to perform (%s): "
					 def-type)
				 cscope-search-descriptions
				 nil
				 t
				 nil
				 'cscope-search-type-history
				 def-type))
	  (def-pattern (thing-at-point 'symbol))
	  (pattern (read-string (format "Search for %s (default `%s'): "
					type
					def-pattern)
				nil
				'cscope-search-pattern-history
				def-pattern)))
     (list (cdr (assoc type cscope-search-descriptions))
	   pattern)))
  (when (>= type (length cscope-search-descriptions))
    (error "Invalid search index: %d" type))
  (when (string= pattern "")
    (error "You gave me no pattern to search for!"))
  (let* ((special (cscope-in-special-tree buffer-file-name))
	 (dir (or (cscope-find-cscope-files buffer-file-name)
		  cscope-last-directory
		  (file-name-directory buffer-file-name)))
	 (file-list (concat dir "/cscope.files"))
	 (args (append (if special
			   '("-k"))
		       (if (file-exists-p file-list)
			   (list "-i" file-list)
			 '("-R"))
		       (list "-q"
			     "-f" (concat dir "/cscope.out")
			     "-L" (format "-%d%s" type pattern)))))
    (let* ((desc (format "%s `%s'"
			 (car (elt cscope-search-descriptions type))
			 pattern))
	   (buf-name (format "*Cscope: %s*" desc)))
      (kill-buffer (get-buffer-create buf-name))
      (let* ((buf (get-buffer-create buf-name))
	     (process (apply 'start-process-shell-command
			     buf-name buf "cscope" args)))
	(save-excursion
	  (set-buffer buf)
	  (delete-region (point-min) (point-max))
	  (set (make-local-variable 'cscope-dir) dir)
	  (set (make-local-variable 'cscope-desc) desc)
	  (set (make-local-variable 'cscope-type) type)
	  (set (make-local-variable 'cscope-pattern) pattern))
	(set-process-sentinel process 'cscope-process-sentinel))
      (message "Searching for %s ..." desc))))
   
(provide 'cscope)


;;; Local Variables:
;;; mode: emacs-lisp
;;; prompt-to-byte-compile: nil
;;; end:

