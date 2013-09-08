(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode) (flymake-cursor-mode) (outline-minor-mode)))

;; (setq outline-level '(lambda ()
;;      (let (buffer-invisibility-spec)
;;         (save-excursion
;;           (skip-chars-forward "\t ")
;;           (current-column)))))

;; (setq haskell-folding-regexp "^[0-9A-Za-z'-_]+")

;; (defun haskell-fold ()
;; (interactive)
;; (fold-lines-from-list (negative-index-list (haskell-functions-index-list))))

;; (defun fold-lines-from-list (lines-list)
;; (if (find 'hl buffer-invisibility-spec)
;;     (call-interactively
;;      (remove-from-invisibility-spec 'hl))
;;   (add-to-invisibility-spec 'hl)
;;   (mapcar (lambda (x)
;;             (overlay-put
;;              (make-overlay (elt x 0) (elt x 1))
;;              'invisible 'hl))
;;           lines-list)))

;; (defun negative-index-list (clean-list)
;; (let ((safe-list '())
;;       (second-elt 1))
;;   (mapcar '(lambda (a)
;;              (when (< second-elt (elt a 0))
;;                (add-to-list 'safe-list (vector second-elt (elt a 0))))
;;              (setq second-elt (elt a 1)))
;;           (append clean-list
;;                   (list (vector (point-max) (point-min)))))
;;   (reverse safe-list)))

;; (defun haskell-functions-index-list ()
;;  (let ((first-position (point))
;;        (word-list '())
;;        (index-list '())
;;        (function-at-point))
;;    (goto-char 1)
;;    (while (search-forward-regexp haskell-folding-regexp nil t)
;;      (setq function-at-point (buffer-substring-no-properties (line-beginning-position)
;;                                                              (point)))
;;      (when (or (not (find function-at-point
;;                           word-list
;;                           :test #'equal))
;;                (and (string-match "--[-]*" function-at-point)
;;                     (not (equal function-at-point
;;                                 (car (last word-list)))))
;;                (equal function-at-point
;;                       "import"))
;;        (setq word-list (append word-list
;;                                (list function-at-point)))
;;        (setq index-list (append index-list
;;                                 (list (vector (line-beginning-position)
;;                                               (1+ (line-end-position))))))))
;;    (goto-char first-position)
;;    index-list))

;; (global-set-key [f1] 'haskell-fold)



;; ;; packages printing for haskell
;; (setq haskell-font-lock-symbols t)

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


(provide 'module-haskell)
