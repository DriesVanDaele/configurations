(TeX-add-style-hook "presentation"
 (lambda ()
    (TeX-add-symbols
     '("LINEIF" 2)
     "headcol"
     "rowcol"
     "topline"
     "midline"
     "rowmidlinecw"
     "rowmidlinewc"
     "rowmidlinew"
     "rowmidlinec"
     "bottomline"
     "bottomlinec")
    (TeX-run-style-hooks
     "wrapfig"
     "fancybox"
     "algorithmic"
     "noend"
     "adjustbox"
     "booktabs"
     "colortbl"
     "algorithm"
     "latex2e"
     "beamer10"
     "beamer")))

