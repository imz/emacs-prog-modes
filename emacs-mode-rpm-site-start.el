; site-start script for Emacs, initializes autoloading of
; Autoconf mode
; 
; November 2003
; ott@altlinux.ru

(autoload 'rpm-spec-mode "rpm-spec-mode" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
			      auto-mode-alist))
