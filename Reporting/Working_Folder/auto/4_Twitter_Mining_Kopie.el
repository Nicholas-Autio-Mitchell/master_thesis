(TeX-add-style-hook
 "4_Twitter_Mining_Kopie"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "margin=1in") ("grffile" "space")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "inputenc"
    "fontenc"
    "fixltx2e"
    "graphicx"
    "longtable"
    "float"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "marvosym"
    "wasysym"
    "amssymb"
    "hyperref"
    "minted"
    "geometry"
    "comment"
    "algorithm2e"
    "grffile")
   (LaTeX-add-labels
    "sec-1"
    "sec-1-1"
    "img:funny"
    "sec-1-2"
    "sec-1-3"
    "sec-2"
    "sec-2-1"
    "sec-2-2"
    "sec-2-2-1"
    "sec-2-2-2"
    "TAS"
    "sec-2-2-3"
    "sec-3"
    "sec-3-1"
    "sec-3-1-1"
    "sec-3-2"
    "sec-3-3"
    "table:twitter-data-usage"
    "sec-3-4"
    "tab.tweet-breakdown"
    "sec-3-5"
    "sec-3-6"
    "sec-4"))
 :latex)

