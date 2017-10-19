;; Make macro "\RefEq" known to AUCTeX as a macro requiring a label as the argument. See AUCTeX info Adding Macros. See variable "TeX-symbol-list" (which is buffer local, so you have to issue "C-h v" in the buffer visiting a tex file related to the master file in question) if it worked as intended.
(TeX-add-style-hook
 "MasArThesis"
 (lambda ()
   (TeX-add-symbols
    '("RefEq" TeX-arg-ref)))
 LaTeX-dialect)

;; Add macro "\RefEq" as a macro requiring a single mandatory argument to macro class "font-latex-match-reference-keywords" for syntax highlighting. See AUCTeX info Fontification of macros.
(add-to-list 'font-latex-match-reference-keywords '("refEq" "{"))

;; Make certain macros known to AUCTeX as macros to which to append an empty set of braces.
(TeX-add-style-hook
 "MasArThesis"
 (lambda ()
   (TeX-add-symbols
    '("Beech" nil)
    '("logNlogDcurve" nil)
    '("NWFVA" nil)
    '("Ponderosa" nil)
    '("Spruce" nil)
    '("Topheight" nil)))
 LaTeX-dialect)

;; Make macro "\RefTab" known to AUCTeX as a macro requiring a label as the argument. See AUCTeX info Adding Macros. See variable "TeX-symbol-list" (which is buffer local, so you have to issue "C-h v" in the buffer visiting a tex file related to the master file in question) if it worked as intended.
(TeX-add-style-hook
 "MasArThesis"
 (lambda ()
   (TeX-add-symbols
    '("refTab" TeX-arg-ref)))
 LaTeX-dialect)

;; Add macro "\RefTab" as a macro requiring a single mandatory argument to macro class "font-latex-match-reference-keywords" for syntax highlighting. See AUCTeX info Fontification of macros.
(add-to-list 'font-latex-match-reference-keywords '("refTab" "{"))

;; Make macro "\RefFig" known to AUCTeX as a macro requiring a label as the argument. See AUCTeX info Adding Macros. See variable "TeX-symbol-list" (which is buffer local, so you have to issue "C-h v" in the buffer visiting a tex file related to the master file in question) if it worked as intended.
(TeX-add-style-hook
 "MasArThesis"
 (lambda ()
   (TeX-add-symbols
    '("refFig" TeX-arg-ref)))
 LaTeX-dialect)

;; Add macro "\RefFig" as a macro requiring a single mandatory argument to macro class "font-latex-match-reference-keywords" for syntax highlighting. See AUCTeX info Fontification of macros.
(add-to-list 'font-latex-match-reference-keywords '("refFig" "{"))

;; Make environment "longtabu" known to AUCTeX as a table environemnt requiring:
;; - 
;; - a mandatory argument in which to specify column types,
;; - a caption, which also contains the label, and
;; - a "firsthead", a "head", a "foot", and a "lastfoot" (each terminated by "end...").
;; status: TODO
