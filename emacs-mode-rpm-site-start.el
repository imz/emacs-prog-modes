; site-start script for Emacs, initializes autoloading of
; rpm-spec mode

;; We need to do this manually, unless `update-file-autoloads' or the like
;; were triggered by installing the package.
(autoload 'rpm-spec-mode "rpm-spec-mode" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec\\'" . rpm-spec-mode))
			      auto-mode-alist))

;; If you do not want to automatically fill new empty .spec-files:
;; (setq rpm-spec-initialize-sections nil)
