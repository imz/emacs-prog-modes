; site-start script for Emacs, initializes autoloading of
; PHP mode
; 
; February 2002
; imz@altlinux.ru

(autoload 'php-mode "php-mode" "Major mode for PHP scripts." t)
(add-to-list 'auto-mode-alist '("\\(\\.php[34]?\\|\\.phtml\\)$" . php-mode))
(add-to-list 'interpreter-mode-alist '("php" . php-mode))

; End of site-start.d/php.el
