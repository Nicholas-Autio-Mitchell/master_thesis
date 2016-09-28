(TeX-add-style-hook
 "3_Sentiment_Analysis"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("footmisc" "bottom") ("geometry" "margin=1in") ("grffile" "space")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
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
    "listingsutf8"
    "footmisc"
    "blindtext"
    "geometry"
    "comment"
    "algorithm2e"
    "grffile")
   (LaTeX-add-labels
    "sec-1"
    "sent-defn"
    "sec-1-1"
    "sent-scores"
    "sec-1-2"
    "tab:example-tuples"
    "SA-limits"
    "sec-1-3"
    "sent-anal"
    "sec-1-4"
    "emolex"
    "sec-1-4-1"
    "fig:wheel-of-emotion"
    "sec-1-4-2"
    "sec-1-4-3"
    "vader"
    "sec-1-4-4"
    "sec-1-4-5"
    "tab:tweet-examples"
    "sec-1-5"
    "sec-1-5-1"
    "sec-1-5-2"
    "sec-1-5-3")
   (LaTeX-add-bibliographies
    "references"))
 :latex)

