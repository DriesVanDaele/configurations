(defvar dir (file-name-directory load-file-name)
  "The root dir of my config distribution.")
(defvar modules-dir (expand-file-name  "modules" dir)
  "This directory contains all external module configurations.")
(defvar modules-file (expand-file-name "modules.el" dir)
  "This file contains a list of modules that will be loaded during Emacs startup.")

(add-to-list 'load-path modules-dir)

(when (file-exists-p modules-file)
  (load modules-file))

;; emacs server (use with emacsclient)
(server-start)

;; elscreen
;; (elscreen-start)

(require 'dired+)

;; some simpler keybindings for buffer switching (rarely used, consideral removal)
;; (windmove-default-keybindings 'meta)

;; (define-abbrev-table 'global-abbrev-table '(
;;     ("alpha" "α")
;;     ("beta" "β")
;;     ("gamma" "γ")
;;     ("theta" "θ")
;;     ("inf" "∞")
;;     ("ar1" "→")
;;     ("ar2" "⇒")
;;     ))

;; (require 'lacarte)
;; (global-set-key [?\e ?\M-x] 'lacarte-execute-command)
;; (global-set-key [?\M-`] 'lacarte-execute-command)

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

