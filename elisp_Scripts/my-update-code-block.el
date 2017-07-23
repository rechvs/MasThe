;; Function for updating code blocks in "DataSetCreation.R" (necessary for incoporating the beech data).
(defun my-update-code-block ()
  (interactive)
  (backward-paragraph)
  (replace-string "gmax_" "gmax_merged_")
  (backward-paragraph)
  (replace-string "#############################" "####################################")
  (backward-paragraph)
  (replace-regexp "\"bart.spruce.clean.1.\\([0-9]\\)" "\"bart.SPECIES.clean.1.\\1")
  (backward-paragraph)
  (replace-regexp "bart.spruce.clean.1.\\([0-9]\\[\\[[\"a-zA-Z0-9.]*]]\\)[ ]*\"" "bart.SPECIES.clean.1.\\1\"")
  (backward-paragraph)
  (replace-regexp "## Create untampered source version of \"bart.SPECIES.clean.1.\\([0-9]\\)\".
bart.spruce.clean.1.[0-9] <- bart.spruce.clean.1.0"
	        "## Loop over all species.
for (cur.species.name in c(\"beech\", \"spruce\")) {
    ## Create untampered source version of \"bart.SPECIES.clean.1.\\1\".
    cur.bart.clean.1.\\1 <- get(x = paste0(\"bart.\", cur.species.name, \".clean.1.0\"))")
  (backward-paragraph)
  (replace-regexp "\\([^\"]\\)bart.spruce.clean.1.\\([0-9]\\)\\([^\"]\\)"
	        "\\1cur.bart.clean.1.\\2\\3")
  (backward-paragraph)
  (replace-regexp "## Add \"bart.SPECIES.clean.1.\\([0-9]\\)\" to the vector of names of objects meant to be saved.
kgmaxObjects <- c(\"bart.SPECIES.clean.1.\\([0-9]\\)\", kgmaxObjects)"
	        "## Assign cur.bart.clean.1.\\1 to the respective object meant to be saved.
    assign(x = paste0(\"bart.\", cur.species.name, \".clean.1.\\1\"),
           value = cur.bart.clean.1.\\1)
    ## Add \"bart.SPECIES.clean.1.\\1\" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(paste0(\"bart.\", cur.species.name, \".clean.1.\\1\"), kgmaxObjects)
}")
  (backward-paragraph)
  (replace-regexp "bart.spruce.clean.1.\\([0-9]\\)"
	        "cur.bart.clean.1.\\1")
  (backward-paragraph)
  (search-forward "## Save results.")
  (insert "
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = \".beech\", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = \".spruce\", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)")
  (mark-paragraph)
  (indent-for-tab-command)
  )
