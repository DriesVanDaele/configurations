;; this one doesn't work, find explanation later.
(autoload 'eclipse-mode "/usr/share/emacs/site-lisp/eclipse_emacs/eclipse.el" "ECLIPSE editing mode" t)
(setq auto-mode-alist (cons '("\\.ecl$" . eclipse-mode) auto-mode-alist))

(provide 'module-eclipse)
