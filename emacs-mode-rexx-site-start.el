; site-start script for Emacs, initializes autoloading of
; Autoconf mode
; 
; November 2003
; ott@altlinux.ru

(autoload 'rexx-mode "rexx-mode" "REXX mode" nil t)
(setq auto-mode-alist
      (append
       (list (cons "\\.rexx$"  'rexx-mode)
	     (cons "\\.elx$"   'rexx-mode)
	     (cons "\\.ncomm$" 'rexx-mode)
	     (cons "\\.cpr$"   'rexx-mode)
	     )
       auto-mode-alist))
