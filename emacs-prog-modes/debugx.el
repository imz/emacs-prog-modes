;;; debugx.el --- Emacs Lisp Debugger eXtensions.
;;;
;;;   Copyright (C) 2002 Helmut Eller
;;;  
;;; debugx.el is copyright 2002 by me, Helmut Eller, and may be used
;;; for any purpose by anyone.  It has no warranty whatsoever.  I
;;; would appreciate acknowledgment if you use it, and I would also
;;; very much appreciate any feedback or bug fixes.
;;;
;;; $Id: debugx.el,v 0.1 2002/12/08 17:20:38 helmut Exp $

;;;; Commentary
;;;
;;; This file implements some useful debugger commands, in particular
;;; a command to highlight the source code expression corresponding to
;;; the currently debugged frame.
;;;
;;; To see how that works: load or evaluate this file; enable the
;;; debugger via `M-x toggle-debug-on-error'; eval (debugx-example).
;;; This will put you into the debugger and you can press `x' to
;;; highlight the buggy expression.
;;;
;;; Commands:
;;;
;;; `x' or `M-x debugx-show-expression' 
;;;    Search and highlight the source expression for the current
;;;    frame.  The search is heuristic and may only find an enclosing
;;;    expression.
;;;
;;; `D' or `M-x debugx-display-current-buffer' 
;;;    Display the "current-buffer" of the debugged program.
;;;
;;; `p' or `M-x debugx-display-pp'
;;;    Eval an expression and pretty print the result.   
;;;
;;; Installation:
;;;
;;; 1. Put this file into your load-path.
;;;
;;; 2. Add this to your .emacs:
;;;
;;;     (add-hook 'debugger-mode-hook (lambda () (require 'debugx)))

;; Bugs:
;;
;; - doesn't work inside byte-compiled functions.
;; - may not work in Emacs 21.

(eval-and-compile
  (require 'cl)
  (require 'debug)
  (require 'thingatpt)
  (require 'pp))

(defun debugx-display-current-buffer ()
  "Display the \"current-buffer\" of the debugged program."
  (interactive)
  (debugger-eval-expression 
   '(switch-to-buffer-other-window (current-buffer))))

(defun debugx-display-current-buffer-other-window ()
  (interactive)
  (debugger-eval-expression 
   '(display-buffer (current-buffer) t)))

(defun debugx-display-pp (%%%form)
  "Eval an expression and pretty print the result."
  (interactive
   (list (read-from-minibuffer "pprint: "
			       nil read-expression-map t
			       'read-expression-history)))
  (debugger-eval-expression 
   `(with-output-to-temp-buffer "*pprint*" 
      (pp ,%%%form))))

(defvar debugx-overlay '()
  "List of overlays inserted by debugx.")

(defun debugx-delete-overlay ()
  (mapc #'delete-overlay debugx-overlay)
  (setq debugx-overlay '()))
  
(defun debugx-highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (debugx-delete-overlay)
  (let ((start (or start (point)))
	(end (or end (save-excursion (forward-sexp) (point)))))
    (setq debugx-overlay 
	  (list (make-overlay start (1+ start))
		(make-overlay (1- end) end)))
    (dolist (overlay debugx-overlay)
      (overlay-put overlay 'face 'secondary-selection))))

(defun debugx-find-pattern (exp pattern test &optional path)
  ;; Return the list of sub-expressions of EXP matching PATTERN.
  ;; Comparison is done with TEST.  The results includes the path to
  ;; the subexpression.  E.g.:
  ;; (debugx-find-pattern '(1 2 (3 4) 3 4) 4 'equal)
  ;;  => ((4 (cdr cdr car cdr car)) (4 (cdr cdr cdr cdr car)))
  (cond ((funcall test exp pattern)
	 (list (list exp (reverse path))))
	((consp exp) 
	 (append (debugx-find-pattern (car exp) pattern test `(car .,path))
		 (debugx-find-pattern (cdr exp) pattern test `(cdr .,path))))))

(defun debugx-partial-match-p (exp pattern)
  (and (consp exp)
       (eq (car exp) (car pattern))
       (= (length exp) (length pattern))))

(defun debugx-walk-path (path)
  (down-list 1)
  (dolist (action path)
    (ecase action
      (car (down-list 1))
      (cdr (forward-sexp))))
  (up-list 1)
  (backward-list))

(defun length=1 (sequence)
  (= (length sequence) 1))

(defun debugx-highlight-subexp (exp subexp)
  "Highlight SUBEXP.

Assume SUBEXP is a unique sub-expression of EXP.  Further assume that
point is positioned immediately before EXP, i.e. -!- <exp>.

E.g: (debugx-highlight-subexp '(progn (/ (+ 1 2) 3)) '(+ 1 2))"
  (let ((matches (debugx-find-pattern exp subexp #'equal)))
    (assert (length=1 matches))
    (debugx-walk-path (cadar matches))
    (recenter (/ (window-height (get-buffer-window (current-buffer))) 2))
    (debugx-highlight-sexp)))

(defstruct (debugx-frame (:type list))
  evaluated-p
  function)
 
(defun debugx-search-closest-subexp (exp partial-backtrace)
  ;; Try to find the smallest subexpression of EXP that corresponds to
  ;; a frame in PARTIAL-BACKTRACE.
  (let ((subexp exp)
	(rest partial-backtrace))
    ;; First identify the smallest proper subexpression of EXP that
    ;; occurs in the backtrace.
    (loop for frame in partial-backtrace 
	  for i from 0 do 
	  (when (and (not (debugx-frame-evaluated-p frame))
		     (length=1 (debugx-find-pattern exp (cdr frame) #'equal)))
	    (setq subexp (cdr frame))
	    (setq rest (subseq partial-backtrace 0 i))
	    (return)))
    ;; Then try a smaller but only partially matching expression
    ;; (corresponding to frames with evaluated arguments).  If only
    ;; one such expression exists take it; otherwise try a larger
    ;; expression.
    (loop for frame in rest do 
	  (when (debugx-frame-evaluated-p frame)
	    (let ((guess (debugx-find-pattern (cdr subexp) (cdr frame)
					      #'debugx-partial-match-p)))
	      (when (length=1 guess)
		(setf subexp (caar guess))
		(return nil)))))
    subexp))
	
(defun debugx-find-function-other-window (function-name)
  (destructuring-bind (buffer &rest point)
      (find-function-noselect function-name)
    (switch-to-buffer-other-window buffer)
    (goto-char point)))

(defun debugx-lose (def)
  (error "%s is %s"
	 def
	 (cond ((or (stringp def) (vectorp def))
		"a keyboard macro")
	       ((subrp def)
		"a built-in function")
	       ((byte-code-function-p def)
		"a byte-compiled function")
	       ((symbolp def)
		(while (symbolp (symbol-function def))
		  (setq def (symbol-function def)))
		(format "an alias for `%s'" def))
	       ((eq (car-safe def) 'lambda)
		"an anonymous function")
	       ((eq (car-safe def) 'macro)
		"a macro"))))

(defun debugx-show-expression-2 (partial-backtrace)
  (let* ((top-level-frame (car (last partial-backtrace)))
	 (top-level-name (debugx-frame-function top-level-frame)))
    (cond ((symbolp top-level-name)
	   (save-selected-window
	     (debugx-find-function-other-window top-level-name)
	     (let* ((exp (sexp-at-point))
		    (subexp (debugx-search-closest-subexp exp 
							  partial-backtrace)))
	       (cond (subexp (debugx-highlight-subexp exp subexp))
		     (t (debugx-lose top-level-name))))))
	  (t 
	   (debugx-lose top-level-name)))))

(defmacro debugx-frame-offset ()
  "Number of frames between the current function and the next frame
for the function `debug'."
  `(do ((i 0 (1+ i)))
       ((eq 'debug (debugx-frame-function (backtrace-frame i)))
	i)))

(progn 
  (defun debugx-show-expression ()
    "Search and highlight the source expression for the current frame."
    (interactive)
    ;; First we copy the backtrace down to the first top-level
    ;; function.  Frames for primitives and top-level functions have
    ;; their arguments evaluated.  We use this to find the top-level
    ;; function.
    (let* ((partial-backtrace
	    ;; Collect all frames down to the second evaluated frame.
	    (loop for i from (ecase emacs-major-version
			       (20 (+ (debugx-frame-offset) 
				      1 
				      (debugger-frame-number)))
			       (21 (debugger-frame-number)))
		  for frame = (backtrace-frame i)
		  while frame
		  count (debugx-frame-evaluated-p frame) into counter
		  collect frame
		  until (= 2 counter))))
      ;; If the topmost (youngest) frame was not evaluated, drop
      ;; everything older than the first evaluated frame.
      (when (not (debugx-frame-evaluated-p (car partial-backtrace)))
	(loop for cell on (cdr partial-backtrace) 
	      for (frame) = cell
	      do (when (debugx-frame-evaluated-p frame)
		   (setcdr cell nil)
		   (return))))
      ;; Now the easy part...
      (debugx-show-expression-2 partial-backtrace)))

  ;; Compile `debugx-show-expression' because the argument to
  ;; `backtrace-frame' is relative to the current frame and we try to
  ;; call `backtrace-frame' and `debugx-frame-offset' in the same
  ;; frame.  This is easier with compiled than with interpreted code.
  (eval-when (eval)
    (byte-compile 'debugx-show-expression)))

(defun debugx-top-level ()
  "Exit all recursive editing levels."
  (interactive)
  (debugx-delete-overlay)
  (top-level))

(defun debugx-example ()
  (let ((bar 0)
	(foo 1))
    (1+ nil)))

(define-key debugger-mode-map "D" 'debugx-display-current-buffer)
(define-key debugger-mode-map "o" 'debugx-display-current-buffer-other-window)
(define-key debugger-mode-map "p" 'debugx-display-pp)
(define-key debugger-mode-map "x" 'debugx-show-expression)
(define-key debugger-mode-map "g" 'find-function-other-window)
(define-key debugger-mode-map "q" 'debugx-top-level)

(add-hook 'debugger-mode-hook #'debugx-delete-overlay)

(run-hooks 'debugx-load-hook)

(provide 'debugx)

;;; debugx.el ends here

