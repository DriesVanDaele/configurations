;; Prolog-mode

;; (add-to-list 'load-path "~/.emacs.d/")
;;            (require 'prolog)
;; (setq auto-mode-alist
;;   (cons (cons "\\.pl" 'prolog-mode)
;;      auto-mode-alist))





(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))
(provide 'module-prolog)
