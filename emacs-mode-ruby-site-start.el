; site-start script for Emacs, initializes autoloading of
; Autoconf mode
; 
; November 2003
; ott@altlinux.ru

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")

(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
	     ))

(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))

(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
    				     interpreter-mode-alist))
