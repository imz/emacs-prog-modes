; site-start script for Emacs, initializes autoloading of
; Autoconf mode
; 
; November 2003
; ott@altlinux.ru

(autoload 'vrml-mode "vrml" "VRML mode." t)
(setq auto-mode-alist (append '(("\\.wrl\\'" . vrml-mode))
			      auto-mode-alist))
