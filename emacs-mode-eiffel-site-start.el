; site-start script for Emacs, initializes autoloading of
; Autoconf mode
; 
; November 2003
; ott@altlinux.ru

(add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-mode))
(autoload 'eiffel-mode "eiffel" "Major mode for Eiffel programs" t)

