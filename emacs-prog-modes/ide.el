;;; ide-mode.el --- minor mode for creating c-based makefiles

;; V.0.2
;;
;; (C) 1998 James Honeyball
;;
;; NOTE: Read the commentary below for the right way to submit bug reports!
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; This file may contain certain bugs.  Please contact ma4jph@bath.ac.uk
;;
;; This package provides a mode in GNU Emacs for managing c-sourcefile
;; projects.  Targets may be created managed and 'made'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ide-target-file-alist (list)
  "List of the current targets source files.")
(defvar ide-mode-map nil
  "Keymap for ide minor mode.")
(defvar ide-project-menu-map nil
  "Menu for project stuff.")
(defvar ide-target-executable nil
  "Executable name for target.")
(defvar ide-target-buffer nil
  "Buffer representing current target file.")
(defvar ide-target-directory nil
  "Directory for current target.")
(defvar ide-project-target-alist (list)
  "List of the current projects targets.")
(defvar ide-project-name nil
  "Current project name.")
(defvar ide-target-include-directories-alist (list)
  "Included directories for current target.")
(defvar ide-compiler-flags "-c $(INCLUDE)"
  "Compiler flags for current target.")
(defvar ide-compiler "cc"
  "*Compiler for use with current target")

(put 'ide-remove-file-from-target 'menu-enable '(car ide-target-file-alist))
(put 'ide-rename-file-from-target 'menu-enable '(car ide-target-file-alist))
(put 'ide-add-file-to-target 'menu-enable 'ide-target-executable)
(put 'ide-edit-makefile 'menu-enable 'ide-target-executable)
(put 'ide-call-make 'menu-enable '(and ide-target-executable
				   (car ide-target-file-alist)))
(put 'ide-create-makefile 'menu-enable '(and ide-target-executable
					 (car ide-target-file-alist)))
(put 'ide-open-target 'menu-enable 'ide-project-name)
(put 'ide-delete-target 'menu-enable '(car ide-project-target-alist))
(put 'ide-new-target 'menu-enable '(car ide-project-target-alist))
(put 'ide-close-project 'menu-enable 'ide-project-name)
(put 'ide-rename-project 'menu-enable 'ide-project-name)

;(put 'print-region 'menu-enable 'mark-active)

(if ide-project-menu-map
    nil
  (setq ide-project-menu-map (make-sparse-keymap "project-menu-map"))
  (define-key ide-project-menu-map [delete-target]
    '("Delete Target..." . ide-delete-target))
  (define-key ide-project-menu-map [new-target]
    '("Create New Target..." . ide-new-target))
  (define-key ide-project-menu-map [open-target]
    '("Open/Select Target..." . ide-open-target))
  (define-key ide-project-menu-map [target-file-seperator]
    '("--"))
  (define-key ide-project-menu-map [rename-project]
    '("Rename Project..." . ide-rename-project))
  (define-key ide-project-menu-map [close-project]
    '("Close Project" . ide-close-project))
  (define-key ide-project-menu-map [open-project]
    '("Open Project..." . ide-open-project)))


(defvar ide-t-menu-map nil
  "Menu for target stuff.")

(if ide-t-menu-map
    nil
  (setq ide-t-menu-map (make-sparse-keymap "t-menu-map"))
  (define-key ide-t-menu-map [rename-file-from-target]
    '("Rename File..." . ide-rename-file-from-target))
  (define-key ide-t-menu-map [remove-file-from-target]
    '("Remove File..." . ide-remove-file-from-target))
  (define-key ide-t-menu-map [add-file-to-target]
    '("Add File..." . ide-add-file-to-target)))

(defvar ide-make-menu-map nil
  "Menu for target stuff.")

(if ide-make-menu-map
    nil
  (setq ide-make-menu-map (make-sparse-keymap "make-menu-map"))
  (define-key ide-make-menu-map [edit-makefile]
    '("Edit makefile" . ide-edit-makefile))
  (define-key ide-make-menu-map [call-make]
    '("Build current target" . ide-call-make))
  (define-key ide-make-menu-map [create-makefile]
    '("Create makefile" . ide-create-makefile)))

(defvar ide-include-menu-map nil
  "Menu for changing include directories.")

(if ide-include-menu-map
    nil
  (setq ide-include-menu-map (make-sparse-keymap "include-menu-map"))
  (define-key ide-include-menu-map [rename-include-directory]
    '("Rename incldue directory..." . ide-rename-include-directory))
  (define-key ide-include-menu-map [remove-include]
    '("Remove include directoy..." . ide-remove-include-directory))
  (define-key ide-include-menu-map [add-include]
    '("Add include directory..." . ide-add-include-directory)))

(defvar ide-flag-menu-map nil
  "Menu for flags and variables stuff.")

(if ide-flag-menu-map
    nil
  (setq ide-flag-menu-map (make-sparse-keymap "flag-menu-map"))
  (define-key ide-flag-menu-map [compiler-flags]
    '("Edit compiler flags" . ide-edit-compiler-flags))
  (define-key ide-flag-menu-map [include]
    (cons "Include directories" ide-include-menu-map))
  (define-key ide-flag-menu-map [change-comiler]
    '("Change compiler" . ide-change-compiler)))

(defvar ide-dynamic-menu-map nil
  "Menu containing source files.  Select option to open file.")

(if ide-dynamic-menu-map
    nil
  (setq ide-dynamic-menu-map (make-sparse-keymap "ide-dynamic-menu-map")))

(defun ide-update-dynamic-menu ()
  "Update dynamic source files."
  (interactive)
  (define-key ide-menu-map [dynamic]
    (cons "Dynamic" nil))
  (setq ide-dynamic-menu-map nil)
  (setq ide-dynamic-menu-map (make-sparse-keymap "ide-dynamic-menu-map"))
  (define-key ide-menu-map [dynamic]
    (cons "Dynamic" ide-dynamic-menu-map))
  (let ((tfiles ide-target-file-alist))
    (define-key ide-dynamic-menu-map [title-sep]
      '("--"))
    (define-key ide-dynamic-menu-map [title]
      (cons (file-name-sans-extension (format "%s" ide-target-buffer)) nil))
    (while (car tfiles)
      (let* ((fn (car (read-from-string 
		       (format "ide-dynamic-%s" (car (car tfiles))))))
	     (body (list 'find-file (format "%s" (cdr (car tfiles)))))
	     (box (format "[%s]" (car (car tfiles)))))
	(eval (list 'defun fn (list) '"h" 
		    '(interactive) body '(setq ide-mode t)))
	(eval (list 'define-key-after
		    'ide-dynamic-menu-map (car (read-from-string box))
		    ``,(cons (format "%s" (car (car tfiles))) fn)
		    ''title-sep)))
      (setq tfiles (cdr tfiles)))))

(defvar ide-menu-map nil
  "Menu for ide mode.")

(if ide-menu-map
    nil
  (setq ide-menu-map (make-sparse-keymap "ide"))
  (define-key ide-menu-map [dynamic]
    (cons "Dynamic" ide-dynamic-menu-map))
  (define-key ide-menu-map [flag]
    (cons "Flags" ide-flag-menu-map))
  (define-key ide-menu-map [make]
    (cons "Make" ide-make-menu-map))
  (define-key ide-menu-map [target]
    (cons "Target" ide-t-menu-map))
  (define-key ide-menu-map [project]
    (cons "Project" ide-project-menu-map)))

(if ide-mode-map
    nil          ; do nothing if ide-mode-map exists allready
  (setq ide-mode-map (make-sparse-keymap))
  (define-key ide-mode-map "\C-x\C-[" 'ide-insert-time)
  (define-key ide-mode-map "\C-x\C-]" 'ide-insert-date)
  (define-key ide-mode-map [control tab] 'dabbrev-expand)
  (define-key ide-mode-map "\C-cm" 'ide-create-makefile)
  (define-key ide-mode-map [menu-bar ide]
              (cons "IDE" ide-menu-map)))

(defvar ide-mode nil
  "Mode variable for ide minor mode.")
(make-variable-buffer-local 'ide-mode)

(defun ide-mode (&optional arg)
  "Minor mode implememnting ide - Intergrated Development Environment.
Special commmands:
\\{ide-mode-map}"
  (interactive "P")
  (setq ide-mode
	(if (null arg)
	    (not ide-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if ide-mode
      ()
    ()))

(if (not (assq 'ide-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(ide-mode " ide")
		minor-mode-alist)))
      
(if (not (assq 'ide-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'ide-mode ide-mode-map)
		minor-mode-map-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mode functions

(defvar insert-time-format "%X"
  "*Format for \\[ide-insert-time] (c.f. 'format-time-string').")
(defvar insert-date-format "%x"
  "*Format for \\[ide-insert-date] (c.f. 'format-time-string').")

(defun ide-source-to-object (string)
  "Converts a source filename to the coresponding object filename."
  (message "%s" string)
  (if (not (stringp string))
      (setq string (format "%s" string)))
  (string-match ".c" string )
  (replace-match ".o " nil t string))

(defun ide-create-makefile (&optional details)
  "Create makefile from current project."
  (interactive)
  (setq debug-on-error (not nil))
  (save-excursion
    (save-match-data
      (generate-new-buffer "makefile")
      (set-buffer "makefile")
      (erase-buffer)
      (princ "INCLUDE = "
	     (get-buffer "makefile"))
      (let ((dirs ide-target-include-directories-alist))
	(while (car dirs)
	  (princ " -I "
		 (get-buffer "makefile"))
	  (princ (cdr (car dirs))
		 (get-buffer "makefile"))
	  (setq dirs (cdr dirs))))
      (princ "\n"
	     (get-buffer "makefile"))
      (princ "CFLAGS = "
	     (get-buffer "makefile"))
      (princ ide-compiler-flags
	     (get-buffer "makefile"))
      (princ "\n"
	     (get-buffer "makefile"))
      (princ "CC = "
	     (get-buffer "makefile"))
      (princ ide-compiler
	     (get-buffer "makefile"))
      (princ "\n"
	     (get-buffer "makefile"))
      (princ ide-target-executable
	     (get-buffer "makefile"))
      (princ " : "
	     (get-buffer "makefile"))
      (let ((tfiles ide-target-file-alist))
	(while (car tfiles)
	  (princ (ide-source-to-object (car (car tfiles)))
		 (get-buffer "makefile"))
	  (setq tfiles (cdr tfiles)))
	(princ "\n\t$(CC) -g -o $@ "
	       (get-buffer "makefile")))
      (let ((tfiles ide-target-file-alist))
	(while (car tfiles)
	  (princ (ide-source-to-object (car (car tfiles)))
		 (get-buffer "makefile"))
	  (setq tfiles (cdr tfiles)))
	(princ "\n"
	       (get-buffer "makefile")))
      (let ((tfiles ide-target-file-alist))
	(while (car tfiles)
;directory stuff	  (princ (file-name-directory (car (car tfiles)))
;currently not used		 (get-buffer "makefile"))
	  (let ((idirs (list))
		(dirs ide-target-include-directories-alist))
		(while (car dirs)
		  (setq idirs (append (list (format "-I%s" (cdr (car dirs))))
				       idirs))
		  (setq dirs (cdr dirs)))
		(setq idirs (append idirs (list "-M" (cdr (car tfiles)))))
		(eval (append '(call-process 
				ide-compiler nil (get-buffer "makefile") nil)
			      idirs)))
	  (princ "\t$(CC) $(CFLAGS) "
		 (get-buffer "makefile"))
	  (princ (car (car tfiles))
		 (get-buffer "makefile"))
	  (princ "\n"
		 (get-buffer "makefile"))
	  (setq tfiles (cdr tfiles))))
      (set-buffer "makefile")
      (make-variable-buffer-local 'make-backup-files)
      (setq make-backup-files nil)
      (write-file "makefile")
      (kill-buffer "makefile"))))

(defun ide-insert-time ()
  "Insert curent time"
  (interactive "*")
  (insert (format-time-string insert-time-format (current-time))))

(defun ide-insert-date ()
  "Insert curent time"
  (interactive "*")
  (insert (format-time-string insert-date-format (current-time))))

(defun ide-call-make ()
  "Calls the make shell program."
  (interactive)
  (let ((buffer (get-buffer-create "make-error")))
    (set-buffer buffer)
    (save-excursion
      (goto-char (point-max))
      (insert "--" )
      (ide-insert-time)
      (insert "--" )
      (ide-insert-date)
      (insert "-----------------------------------------------------------\n")
      (message "make running")
      (let ((process 
	     (start-process "make"
			    buffer
			    "make")))
	(set-process-sentinel process 'ide-make-sentinel)))))

(defun ide-make-sentinel (process string)
  "When make is finished insert date-line in make-error."
  (if (eq (process-status process)
	  'exit)
      (progn
	(let* ((buffer (get-buffer-create "make-error")))
	  (set-buffer buffer)
	  (save-excursion
	    (goto-char (point-max))
	    (insert "--" )
	    (ide-insert-time)
	    (insert "--" )
	    (ide-insert-date)
	    (insert "------------------------------")
	    (insert "-----------------------------\n\n")
	    (message "make finished"))))))

(defun ide-edit-makefile ()
  "Edit makefile using makefile-mode."
  (interactive)
  (find-file "makefile")
  (ide-mode))

(defun ide-open-target ()
  "Open target."
  (interactive)
  (let ((name (completing-read "Open target: "
			       ide-project-target-alist nil t)))
    (setq ide-target-buffer (find-file-noselect 
			     (cdr (assoc name ide-project-target-alist))))
    (let ((m (set-marker (make-marker) 1 ide-target-buffer)))
      (set-buffer ide-target-buffer)
      (goto-char (point-min))
      (message "%d %d %d" 
	       (marker-position m) 
	       (point-max) 
	       (marker-position (point-max-marker)))
      (setq ide-target-directory (read m))
      (setq ide-target-executable (read m))
      (move-marker m (re-search-forward "^\$(CFLAGS) =" nil t))
      (setq ide-compiler-flags (read m))
      (move-marker m (re-search-forward "^\$(CC) =" nil t))
      (setq ide-compiler (read m))
      (if (not (stringp ide-compiler))
	  (setq ide-compiler (format "%s" ide-compiler)))
      (move-marker m (re-search-forward "^\$(INCLUDE) =" nil t))
      (setq ide-target-include-directories-alist (list))
      (let ((tok (read m)))
	(setq tok (if (not (stringp tok))
		      (format "%s" tok)))
	(while (not (equal tok "//"))
	  (message "inc %s" tok)
	  (setq ide-target-include-directories-alist
		(append (list (cons (file-relative-name tok)
				    (expand-file-name tok)))
			ide-target-include-directories-alist))
	  (setq tok (read m))
	  (setq tok (if (not (stringp tok))
			(format "%s" tok)))
	  (message "tok is \"%s\"" tok)))
      (setq ide-target-file-alist (list))
      (while (not (or (equal (marker-position m) (- (point-max) 1))
		      (equal (marker-position m) (point-max))))
      (let ((temp-filename (read m)))
	(setq temp-filename (if (not (stringp temp-filename))
				(format "%s" temp-filename)))
	(message "ide %s" temp-filename)
	(setq ide-target-file-alist
              (append (list (cons (file-name-nondirectory
				   temp-filename)
                                  temp-filename))
                      ide-target-file-alist))
 	(message "%d %d" (marker-position m) (point-max)))))
    (ide-update-dynamic-menu)))

(defun ide-add-file-to-target (name)
  "Add a source file to the current target."
  (interactive "fAdd file: ")
  (let ((name (format "%s" (expand-file-name name))))
    (if (not (rassoc name ide-target-file-alist))
	(progn
	  (setq ide-target-file-alist 
		(append (list (cons (file-name-nondirectory
				     name)
				    name))
			ide-target-file-alist))
	  (set-buffer (get-buffer-create ide-target-buffer))
	  (goto-char (point-max))
	  (princ name (get-buffer ide-target-buffer))
	  (princ "\n" (get-buffer ide-target-buffer))
	  (save-buffer ide-target-buffer)
	  (ide-update-dynamic-menu)
	  (message "%s added to %s" name ide-target-buffer))
      (message "%s allready a member" name))))

(defun ide-remove-file-from-target ()
  "Remove source file from current target."
;  (interactive "FRemove file: ")
  (interactive)
  (let ((name (completing-read "Remove Source: "
			       ide-target-file-alist nil t)))
;  (let ((name (format "%s" (expand-file-name name))))
    (if (not (member (assoc (file-name-nondirectory name) 
			    ide-target-file-alist) ide-target-file-alist))
	(message "%s is not a member" name)
      (progn
	(setq ide-target-file-alist (delete (assoc 
					     (file-name-nondirectory name)
					     ide-target-file-alist)
					    ide-target-file-alist))
	(save-excursion
	(set-buffer (get-buffer-create ide-target-buffer))
	(save-restriction
	  (save-match-data
	    (widen)
	    (goto-char (point-min))
	    (while (search-forward name nil t)
	      (beginning-of-line)
	      (let ((start (point)))
		(search-forward "\n")
		(delete-region start (match-end 0))))
	    (save-buffer ide-target-buffer)
	    (ide-update-dynamic-menu)
	(message "%s removed from %s" name ide-target-buffer))))))))

(defun ide-rename-file-from-target (oldname newname)
  "Rename source file in current target."
  (interactive "FSource file to rename: \nFNew name for source file: ")
  (ide-remove-file-from-target oldname)
  (ide-add-file-to-target newname)
  (ide-update-dynamic-menu)
  (message "%s renamed to %s in %s" oldname newname ide-target-executable))

(defun ide-open-project (name)
  "Open Project."
  (interactive "FOpen project: ")
  (setq debug-on-error (not nil))
  (let* ((project-buffer (set-buffer (find-file-noselect name)))
	 (m (set-marker (make-marker) 1 project-buffer)))
    (goto-char (point-min))
    (setq ide-project-target-alist (list))
    (message "%d %d %d" 
	   (marker-position m) 
	   (point-max) 
	   (marker-position (point-max-marker)))
    (setq ide-project-name (read m))
    (while (not (or (equal (marker-position m) (- (point-max) 1))
		    (equal (marker-position m) (point-max))))
      (let ((temp-targetname (read m)))
	(setq temp-targetname (if (not (stringp temp-targetname))
				  (format "%s" temp-targetname)))
	(message "ide %s" temp-targetname)
	(setq ide-project-target-alist 
	      (append (list (cons (file-name-sans-extension
			     (file-name-nondirectory
			      temp-targetname))
				  temp-targetname))
		      ide-project-target-alist))
	(message "%s" ide-project-target-alist)
	(message "%d %d" (marker-position m) (point-max))))))

(defun ide-close-project ()
  "Close current Project."
  (interactive)
  (setq ide-project-name nil)
  (setq ide-target-include-directories-alist (list))
  (setq ide-project-target-alist (list)))

(defun ide-add-include-directory (name)
  "Add an include directory."
  (interactive "DAdd directory: ")
  (let ((name (format "%s" (expand-file-name name))))
    (if (not (rassoc name ide-target-include-directories-alist))
	(if (file-accessible-directory-p name)
	    
	    (progn
	      (setq ide-target-include-directories-alist 
		    (append (list (cons (file-relative-name
					 name)
					name))
			    ide-target-include-directories-alist))
	      (set-buffer (get-buffer-create ide-target-buffer))
	      (goto-char (point-min))
	      (let ((m (set-marker (make-marker) 
				   (re-search-forward "^\$(INCLUDE) =")
				   ide-target-buffer)))
		(princ name m)
	  (princ "\n" (get-buffer ide-target-buffer))
		(save-buffer ide-target-buffer)
		(message "%s added to %s" (file-relative-name name)
			 ide-target-buffer)))
	  (message "%s is not accessible as a directory" name))
      (message "%s allready included" name))))


(defun ide-remove-include-directory ()
  "Remove an include directory."
  (interactive)
  (let ((name (completing-read "Remove Source: "
			       ide-target-include-directories-alist nil t)))
    (setq name (expand-file-name name))
    (if (not (rassoc name ide-target-include-directories-alist))
	(message "%s is not included" name)
      (progn
	(setq ide-target-include-directories-alist 
	      (delete (rassoc name ide-target-include-directories-alist)
		      ide-target-include-directories-alist))
;)))))
	(save-excursion
	  (set-buffer (get-buffer-create ide-target-buffer))
	  (save-restriction
	    (save-match-data
	      (widen)
	      (goto-char (point-min))
	      (while (search-forward name nil t)
;		(beginning-of-line)
		(let ((start (match-beginning 0)))
		  (search-forward "\n")
		  (delete-region start (match-end 0))))
	      (save-buffer ide-target-buffer)
	      (message "%s removed from %s" name ide-target-buffer))))))))

(defun ide-change-compiler ()
  "Change compiler.  Needs -M 'dependencies' option."
  (interactive)
  (setq ide-compiler
	(read-from-minibuffer "Compiler to use: " ide-compiler))
  (save-excursion
    (set-buffer (get-buffer-create ide-target-buffer))
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
        (re-search-forward "^\$(CC) =" nil t)
	(insert ide-compiler)
	(let ((start (point)))
	  (search-forward "\n")
	  (delete-region start (match-beginning 0)))
	(save-buffer ide-target-buffer)
	(message "compiler now \"%s\"" ide-compiler)))))
	

(defun ide-edit-compiler-flags ()
  "Change compiler flags."
  (interactive)
  (setq ide-compiler-flags
	(read-from-minibuffer "Compiler options: " ide-compiler-flags))
  (save-excursion
    (set-buffer (get-buffer-create ide-target-buffer))
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
        (re-search-forward "^\$(CFLAGS) =" nil t)
	(insert ide-compiler-flags)
	(let ((start (point)))
	  (search-forward "\n")
	  (delete-region start (match-beginning 0)))
	(save-buffer ide-target-buffer)
	(message "compiler options now \"%s\"" ide-compiler-flags)))))


