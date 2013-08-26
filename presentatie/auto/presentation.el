(TeX-add-style-hook "presentation"
 (lambda ()
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

