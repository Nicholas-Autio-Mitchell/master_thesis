(TeX-add-style-hook
 "2_Twitter_Mining"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("footmisc" "bottom") ("geometry" "a4paper" "margin=1in") ("algorithm2e" "linesnumbered" "ruled" "lined" "shortend") ("grffile" "space")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
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
    "listingsutf8"
    "footmisc"
    "geometry"
    "comment"
    "algorithm2e"
    "grffile")
   (LaTeX-add-labels
    "sec-1"
    "soc-data"
    "sec-1-1"
    "sec-1-1-1"
    "fig:gtrends"
    "sec-1-1-2"
    "sec-1-1-3"
    "sec-1-2"
    "sec-1-2-1"
    "criteria"
    "sec-1-2-2"
    "twitter-sources"
    "sec-1-2-3"
    "sec-1-2-3-1"
    "sec-1-2-3-2"
    "TAS"
    "sec-1-2-3-3"
    "sec-1-3"
    "sec-1-3-1"
    "sec-1-3-1-1"
    "sec-1-3-2"
    "sec-1-3-2-1"
    "iterative-scraping"
    "sec-1-3-3"
    "alg-page-scrape"
    "alg-batch-scrape"
    "html-parsing"
    "sec-1-3-4"
    "sec-1-3-4-1"
    "sec-1-3-4-2"
    "table:twitter-data-usage"
    "cleaning-tweets"
    "sec-1-3-5"
    "final-output"
    "sec-1-3-6"
    "tab.tweet-breakdown"
    "sec-1-4"
    "mycode:tweet-cleaner"))
 :latex)

