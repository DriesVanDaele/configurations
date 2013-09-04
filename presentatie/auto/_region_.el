(TeX-add-style-hook "_region_"
 (lambda ()
    (LaTeX-add-labels
     "evidence-table"
     "confusion-matrix-bun")
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
     "tabularx"
     "algorithm"
     "caption"
     "multirow"
     "upquote"
     "latex2e"
     "beamer10"
     "beamer")))

