; site-start script for Emacs, initializes autoloading of
; Autoconf mode
; 
; November 2003
; ott@altlinux.ru

(autoload 'postscript-mode "postscript.el" "" t)
(setq auto-mode-alist
      (cons '("\\.c?ps$".postscript-mode) auto-mode-alist))
