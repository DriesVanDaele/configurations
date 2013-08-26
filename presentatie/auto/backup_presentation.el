(TeX-add-style-hook "backup_presentation"
 (lambda ()
    (LaTeX-add-labels
     "table1"
     "table2"
     "table3")
    (TeX-add-symbols
     '("LINEIF" 2))
    (TeX-run-style-hooks
     "wrapfig"
     "fancybox"
     "algorithmic"
     "noend"
     "adjustbox"
     "algorithm"
     "latex2e"
     "beamer10"
     "beamer")))

