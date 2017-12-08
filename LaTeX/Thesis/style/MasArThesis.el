;; Make certain macros known to AUCTeX as macros requiring a label as the argument. See AUCTeX info Adding Macros. See variable "TeX-symbol-list" (which is buffer local, so you have to issue "C-h v" in the buffer visiting a tex file related to the master file in question) if it worked as intended.
(TeX-add-style-hook
 "MasArThesis"
 (lambda ()
   (TeX-add-symbols
    '("SeePage" TeX-arg-ref)
    '("SeeSection" TeX-arg-ref)))
 LaTeX-dialect)

;; Add certain macros as macros requiring a single mandatory argument to macro class "font-latex-match-reference-keywords" for syntax highlighting. See AUCTeX info Fontification of macros.
(add-to-list 'font-latex-match-reference-keywords '("SeePage" "{"))
(add-to-list 'font-latex-match-reference-keywords '("SeeSection" "{"))

;; Make certain macros known to AUCTeX as macros to which to append an empty set of braces.
(TeX-add-style-hook
 "MasArThesis"
 (lambda ()
   (TeX-add-symbols
    '("BasalAreaR" nil)
    '("Beech" nil)
    '("edf" nil)
    '("logNlogDcurve" nil)
    '("NWFVA" nil)
    '("Ponderosa" nil)
    '("ProductivityIndexMath" nil)
    '("ProductivityIndexText" nil)
    '("ProductivityIndexVariableMath" nil)
    '("ProductivityIndexVariableR" nil)
    '("ProductivityIndexVariableText" nil)
    '("ProductivityIndexYieldClassIMath" nil)
    '("Spruce" nil)
    '("StandAgeVariableMath" nil)
    '("StandAgeVariableR" nil)
    '("SterbadgGmaxMath" nil)
    '("SterbaGmaxMath" nil)
    '("SterbaNGmaxMath" nil)
    '("TopHeightMath" nil)))
 LaTeX-dialect)

;; Make certain macros known to AUCTeX as macros which require a single mandatory argument.
(TeX-add-style-hook
 "MasArThesis"
 (lambda ()
   (TeX-add-symbols
    '("symbf" 1)))  ;; See "unicode-math" manual, p. 11.
 LaTeX-dialect)

;; Make certain macros known to AUCTeX as macros which require 2 mandatory arguments.
(TeX-add-style-hook
 "MasArThesis"
 (lambda ()
   (TeX-add-symbols
    '("interval" 2)))  ;; See package "interval".
 LaTeX-dialect)

;; Make environment "longtabu" known to AUCTeX as a table environemnt requiring:
;; - 
;; - a mandatory argument in which to specify column types,
;; - a caption, which also contains the label, and
;; - a "firsthead", a "head", a "foot", and a "lastfoot" (each terminated by "end...").
;; status: TODO
