(TeX-add-style-hook
 "4_Gradient_Boosting"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("footmisc" "bottom") ("geometry" "a4paper" "margin=1in") ("algorithm2e" "linesnumbered" "ruled" "lined" "shortend") ("grffile" "space")))
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
    "listingsutf8"
    "enumitem"
    "mathtools"
    "footmisc"
    "geometry"
    "comment"
    "algorithm2e"
    "grffile")
   (LaTeX-add-labels
    "chapter-gradient-boosting"
    "sec-1"
    "AdaBoost"
    "sec-1-1"
    "linear-model"
    "sec-1-2"
    "fig:basic.residuals"
    "grad-boosting"
    "sec-1-3"
    "basic-problem"
    "sec-1-3-1"
    "eqn-additive-model"
    "gen-props"
    "sec-1-3-2"
    "naive-boosting"
    "sec-1-3-3"
    "eqn-optimise"
    "eqn-bin-loss"
    "eqn-gauss-loss"
    "eqn-emp-risk"
    "eqn-gauss-emp-risk"
    "eqn-loss-derivative"
    "eqn-residuals-loss"
    "eqn-naive-increment"
    "fig:contour_plot"
    "comp-boosting"
    "sec-1-4"
    "comp-alg"
    "sec-1-4-1"
    "alg-comp-boosting"
    "eqn-neg-grad-vector"
    "param-selection"
    "sec-1-4-2"
    "nu"
    "sec-1-4-2-1"
    "fig:grad-descent"
    "mstop"
    "sec-1-4-2-2"
    "fig:cvrisk-example"
    "stochastic-boosting"
    "sec-1-5"
    "sec-1-6"
    "sec-1-6-1"
    "eqn-sqaured-error"
    "sec-1-6-2"
    "eqn-bin-error"
    "sec-1-6-3"
    "sec-1-6-4"
    "code:family"))
 :latex)

