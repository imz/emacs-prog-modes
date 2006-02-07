; site-start script for Emacs, initializes autoloading of
; Autoconf mode
; 
; November 2003
; ott@altlinux.ru

(load "c-mode-addons")

(defun my-c-mode-addons-hook ()
  (define-key c-mode-map "\M-s" 'c-synopsis-at-point)
  (define-key c++-mode-map "\M-s" 'c-synopsis-at-point)
  (define-key c-mode-map "\C-ce" 'c-eval-enum)
  (define-key c++-mode-map "\C-ce" 'c-eval-enum)
  (define-key c-mode-map "(" 'c-electric-parenthesis-open)
  (define-key c++-mode-map "(" 'c-electric-parenthesis-open)
  (autoload 'set-c-style "c-style" nil t)
)

(add-hook 'c-mode-common-hook 'my-c-mode-addons-hook)