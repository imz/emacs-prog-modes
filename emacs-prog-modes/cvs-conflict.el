
;;; cvs-conflict.el --- Minor mode for editing cvs conflicts


;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package helps you edit files with cvs conflict tags.
;;
;; To use insert in these lines into your ~/.emacs:
;;
;;  (require 'cvs-conflict)
;;  (add-hook 'find-file-hooks 'cvs-conflict-detect)
;;  (add-hook 'after-revert-hook 'cvs-conflict-detect)
;;
;; cvs-conflict minor mode will be activated automatically if it
;; detects the presence of cvs conflict tags in the buffer. It will
;; automatically deactivate itself if the buffer is saved and no more
;; tags are detected.
;;
;; cvs-conflict was tested with GNU Emacs 20.5 and greater
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'easymenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Variables:

;;; Interface to the command system

(defvar cvs-conflict-mode nil
  "Non-nil means cvs-conflict local minor mode is enabled.")
(make-variable-buffer-local 'cvs-conflict-mode)

(defvar cvs-conflict-mode-menu nil
  "Keymap containing the menu for mode.")

(defgroup cvs-conflict nil
  "Resolve cvs conflicts in a file"
  :link '(emacs-library-link :tag "Source Lisp File" "cvs-conflict.el")
  :group 'faces
  :group 'frames
  :group 'editing)


(defcustom cvs-conflict-current-face 'cvs-conflict-current-face
  "*Symbol face used to highlight conflicting text in current file."
  :type 'face
  :group 'cvs-conflict)

(defcustom cvs-conflict-update-face 'cvs-conflict-update-face
  "*Symbol face used to highlight conflicting text from update."
  :type 'face
  :group 'cvs-conflict)

(defcustom cvs-conflict-separater-face 'cvs-conflict-separater-face
  "*Symbol face used to highlight conflicting text from update."
  :type 'face
  :group 'cvs-conflict)

(defface cvs-conflict-current-face '((t (:background "PaleGreen" :foreground "Black")))
  "Face used to highlight conflicting text in current file.")

(defface cvs-conflict-update-face '((t (:background "Wheat" :foreground "Black")))
  "Face used to highlight conflicting text from update.")

(defface cvs-conflict-separater-face '((t (:background "Grey" :foreground "Black")))
  "Face used to highlight conflicting text separaters.")

(defcustom cvs-conflict-verbose t
  "*Non-nil means generate messages."
  :type 'boolean
  :group 'cvs-conflict)

(defcustom cvs-conflict-invisible-separator nil
  "*Non-nil means make separator invisible."
  :type 'boolean
  :group 'cvs-conflict)

(defcustom cvs-conflict-invisible-version-names nil
  "*Non-nil means make file name/version invisible."
  :type 'boolean
  :group 'cvs-conflict)

(defcustom cvs-conflict-mode-string " Conflict"
  "*The minor mode string displayed when mode is on."
  :type 'string  
  :group 'cvs-conflict)

(defcustom cvs-conflict-separator-regex "^=======$"
  "*Regexp for separataor of conflict region."
  :type 'regexp  
  :group 'cvs-conflict)

(defcustom cvs-conflict-begin-regex "^<<<<<<< "
  "*Regexp for beginning of conflict region."
  :type 'regexp  
  :group 'cvs-conflict)

(defcustom cvs-conflict-end-regex "^>>>>>>> "
  "*Regexp for end of conflict region."
  :type 'regexp  
  :group 'cvs-conflict)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization


(defun cvs-conflict-customize ()
  "Customize cvs-conflict group."
  (interactive)
  (customize-group 'cvs-conflict))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User commands


(defun cvs-conflict-mode (&optional arg)
  "Toggle minor mode to aid in cvs source conflict editing.
With ARG, turn cvs-conflict mode on if ARG is positive, off
otherwise."
  (interactive "P")
  (if (if arg
	  (> (prefix-numeric-value arg) 0)
	(not cvs-conflict-mode))
      (cvs-conflict-on)
    (cvs-conflict-off)))

(defun cvs-conflict-on ()
  "Turn on cvs-conflict-mode."
  (interactive)
  (setq cvs-conflict-mode t)
  (cvs-conflict-highlight-buffer)
  (run-hooks 'cvs-conflict-hook)
  (easy-menu-add cvs-conflict-mode-menu)
  (message "Cvs-Conflict local mode is on"))


(defun cvs-conflict-off ()
  "Turn off cvs-conflict-mode."
  (interactive)
  (setq cvs-conflict-mode nil)
  (cvs-conflict-remove-overlays)
  (easy-menu-remove cvs-conflict-mode-menu)
  (message "Cvs-Conflict local mode is off"))


(defun cvs-conflict-next-conflict ()
  "Find the next conflict region returns 
position on success nil on failure"
  (interactive)
  (goto-char (point-at-eol))
  (if (re-search-forward cvs-conflict-separator-regex nil t)
      (progn (goto-char (point-at-bol)) (point))
	(goto-char (point-at-bol))
	nil))

(defun cvs-conflict-previous-conflict ()
  "Find the previous conflict region 
returns position on success nil on failure"
  (interactive)
  (goto-char (point-at-bol))
  (if (re-search-backward cvs-conflict-separator-regex nil t)
      (progn (goto-char (point-at-bol)) (point))
	(goto-char (point-at-bol))
	nil))

(defun cvs-conflict-beginning-of-conflict ()
  "Move to the beginning of conflict region
position on success nil on failure"
  (interactive)
  (while (looking-at "<")
    (forward-char))
  (while (looking-at " ")
    (forward-char))
  (if (re-search-backward cvs-conflict-begin-regex nil t)
      (progn (goto-char (point-at-bol)) (point))
	(goto-char (point-at-bol))
	nil))

(defun cvs-conflict-end-of-conflict ()
  "Move to the end of conflict region
position on success nil on failure"
  (interactive)
  (if (re-search-forward cvs-conflict-end-regex nil t)
      (progn (goto-char (point-at-bol)) (point))
	(goto-char (point-at-bol))
	nil))

(defun cvs-conflict-kill-separators ()
  "Remove merge separators in current conflicts"
  (interactive)
  (save-excursion
    (cvs-conflict-beginning-of-conflict)
    (kill-line 1)
    (while (not (looking-at cvs-conflict-separator-regex)) (forward-line))
    (kill-line 1)
    (while (not (looking-at cvs-conflict-end-regex)) (forward-line))
    (kill-line 1)))

(defun cvs-conflict-reject-other-version ()
  "Remove merge separators in current conflicts and reject other
version"
  (interactive)
  (save-excursion
    (cvs-conflict-beginning-of-conflict)
    (kill-line 1)
    (while (not (looking-at cvs-conflict-separator-regex)) (forward-line))
    (kill-line 1)
    (while (not (looking-at cvs-conflict-end-regex)) (kill-line 1))
    (kill-line 1)))

(defun cvs-conflict-accept-other-version ()
  "Remove merge separators in current conflicts and accept other
version"
  (interactive)
  (save-excursion
    (cvs-conflict-beginning-of-conflict)
    (kill-line 1)
    (while (not (looking-at cvs-conflict-separator-regex)) (kill-line 1))
    (kill-line 1)
    (while (not (looking-at cvs-conflict-end-regex)) (forward-line))
    (kill-line 1)))

(defun cvs-conflict-kill ()
  "Remove merge separators and all conflicting text"
  (interactive)
  (save-excursion
    (cvs-conflict-beginning-of-conflict)
    (while (not (looking-at cvs-conflict-end-regex)) (kill-line 1))
    (kill-line 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions

(defun cvs-conflict-in-buffer-p ()
  "Detect if a buffer contains any CVS conflict regions"
  (save-excursion
    (goto-char (point-min))
    (if (cvs-conflict-next-conflict)
	(if (cvs-conflict-beginning-of-conflict)
	    (if (cvs-conflict-end-of-conflict) t)))))

(defun cvs-conflict-remove-overlays ()
  "Remove all overlays in buffer"
  (interactive)
  (let ((overlays (overlays-in (point-min) (point-max))))
    (while overlays
      (if (memq (overlay-get (first overlays) 'face) 
		'(cvs-conflict-separater-face
		  cvs-conflict-current-face
		  cvs-conflict-update-face))
	  (delete-overlay (first overlays)))
      (setq overlays (rest overlays)))))

(defun cvs-conflict-highlight-line (highlight-face &optional invis)
  "Highlight line with given FACE."
  (let ((overlay (make-overlay (line-beginning-position)
			       (1+ (line-end-position)))))
    (overlay-put overlay 'hilit t)
    (overlay-put overlay 'invisible invis)
    (overlay-put overlay 'face highlight-face)
    (overlay-put overlay 'priority 0)))


(defun cvs-conflict-highlight-buffer ()
  "Highlight conflicts."
  (interactive)
  (save-excursion
    (cvs-conflict-remove-overlays)
    (let ((count 0))
      (goto-char (point-min))
      (while (cvs-conflict-next-conflict)
	(setq count (1+ count))
	;; highlight the separator
	(cvs-conflict-highlight-line 'cvs-conflict-separater-face
				     cvs-conflict-invisible-separator)
	(if (cvs-conflict-end-of-conflict)
	    (cvs-conflict-highlight-line 'cvs-conflict-separater-face
					 cvs-conflict-invisible-version-names))
	(if (cvs-conflict-beginning-of-conflict)
	    (cvs-conflict-highlight-line 'cvs-conflict-separater-face
					 cvs-conflict-invisible-version-names))
	(forward-line)
	(while (not (looking-at cvs-conflict-separator-regex))
	  (cvs-conflict-highlight-line 'cvs-conflict-current-face)
	  (forward-line))
	(forward-line)
	(while (not (looking-at cvs-conflict-end-regex))
	  (cvs-conflict-highlight-line 'cvs-conflict-update-face)
	  (forward-line)))
      ;(message "%d conflicts" count)
      )))

(defun cvs-conflict-detect ()
  (if (cvs-conflict-in-buffer-p)
      (cvs-conflict-on)
    (if (eq cvs-conflict-mode t)
	(cvs-conflict-off))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cvs-conflict-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cn" 'cvs-conflict-next-conflict)
    (define-key map "\C-cp" 'cvs-conflict-previous-conflict)
    (define-key map "\C-ca" 'cvs-conflict-beginning-of-conflict)
    (define-key map "\C-ce" 'cvs-conflict-end-of-conflict)
    (define-key map "\C-c\d" 'cvs-conflict-kill)
    (define-key map "\C-cr" 'cvs-conflict-reject-other-version)
    (define-key map "\C-co" 'cvs-conflict-accept-other-version)
    (define-key map "\C-cl" 'cvs-conflict-highlight-buffer)
    (define-key map "\C-ck" 'cvs-conflict-kill-separators)
    (easy-menu-define
     cvs-conflict-mode-menu
     (if (boundp 'xemacs-logo)
	 nil
       (list map))
     "Cvs Conflict menu"
     (list
      "Conflict"
      ["Next"		    cvs-conflict-next-conflict	t]
      ["Previous"	    cvs-conflict-previous-conflict	t]
      ["Beginning"	    cvs-conflict-beginning-of-conflict t]
      ["End"		    cvs-conflict-end-of-conflict t]
      "----"
      ["Reject Other Version" cvs-conflict-reject-other-version t]
      ["Use Other Version"  cvs-conflict-accept-other-version t]
      ["Kill Separators"    cvs-conflict-kill-separators t]
      ["Kill Conflict"	    cvs-conflict-kill t]
      "----"
      ["Update"		    cvs-conflict-highlight-buffer t]
      ["Turn Off"	    cvs-conflict-off t] ))
    map)
  "Keymap for cvs-conflict commands")

(add-to-list 'minor-mode-alist 
	     '(cvs-conflict-mode cvs-conflict-mode-string))
(let ((a (assoc 'cvs-conflict-mode minor-mode-map-alist)))
  (if a
      (setcdr a cvs-conflict-mode-map)
    (add-to-list 'minor-mode-map-alist 
		 (cons 'cvs-conflict-mode cvs-conflict-mode-map))))
  
(provide 'cvs-conflict)
(run-hooks 'cvs-conflict-load-hook)

