(TeX-add-style-hook
 "readme"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "utf8") ("hyperref" "unicode=true")))
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
    "lmodern"
    "amssymb"
    "amsmath"
    "ifxetex"
    "ifluatex"
    "fixltx2e"
    "fontenc"
    "inputenc"
    "mathspec"
    "fontspec"
    "upquote"
    "microtype"
    "hyperref"
    "graphicx"
    "grffile"
    "parskip")
   (TeX-add-symbols
    "tightlist"
    "maxwidth"
    "maxheight"
    "oldparagraph"
    "oldsubparagraph")
   (LaTeX-add-labels
    "scheme-language"
    "introduction"
    "implementation-of-scheme"
    "test"
    "our-program"))
 :latex)

