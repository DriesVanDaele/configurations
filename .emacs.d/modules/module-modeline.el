(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-mail-directory nil
      display-time-default-load-average nil
      display-time-mail-file (quote none))

(display-time-mode)
(display-battery-mode)

(provide 'module-modeline)
