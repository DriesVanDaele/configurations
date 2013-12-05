;; place line break at column 65 (combine with auto-fill-mode)
(setq-default fill-column 65)

;; tab width 4 consisting solely of spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Make copy emacs works with other applications
;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


(setq interprogram-cut-function 'paste-to-osx 
      interprogram-paste-function 'copy-from-osx) 

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(defun copy-from-osx () 
  (let ((coding-system-for-read 'utf-8)) 
    (shell-command-to-string "pbpaste"))) 

(defun paste-to-osx (text &optional push) 
  (let ((process-connection-type nil)) 
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy"))) 
      (set-process-sentinel proc 'ignore) ;; stifle noise in *Messages* 
      (process-send-string proc text) 
      (process-send-eof proc))) 
text) 



(provide 'module-text)
