; site-start script for Emacs, initializes autoloading of
; Autoconf mode
; 
; November 2003
; ott@altlinux.ru

(autoload 'xbase-mode "xbase" "Xbase mode" t)
(setq auto-mode-alist (cons (cons "\\.\\(prg\\|ch\\)$" 'xbase-mode) auto-mode-alist))
