;; place line break at column 65 (combine with auto-fill-mode)
(setq-default fill-column 65)

;; tab width 4 consisting solely of spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Make copy emacs works with other applications
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(provide 'module-text)
