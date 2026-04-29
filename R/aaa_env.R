# Environnement interne du package

.trialdesign_env <- new.env(parent = emptyenv())


############################################################
# FONCTION INTERNE .rename_pdf

# Renommage des colonnes techniques en libellés lisibles pour le PDF
.rename_pdf <- function(df) {
  rename_map <- c(
    RDNUM     = "Numéro de randomisation",
    RDGRP     = "Code groupe de randomisation",
    RDGRP_LIB = "Libellé groupe de randomisation",
    RDSTR     = "Code strate",
    RDSTR_LIB = "Libellé strate",
    RDBOI     = "Numéro de boîte de traitement",
    RDBOI_LIB = "Libellé de boîte de traitement"
  )

  # Colonnes RDSTR1, RDSTR_LIB1, RDSTR2, RDSTR_LIB2, etc. (circuit REDCap)
  for (col in names(df)) {
    if (grepl("^RDSTR(\\d+)$", col)) {
      num <- sub("^RDSTR(\\d+)$", "\\1", col)
      rename_map[col] <- paste0("Code strate ", num)
    }
    if (grepl("^RDSTR_LIB(\\d+)$", col)) {
      num <- sub("^RDSTR_LIB(\\d+)$", "\\1", col)
      rename_map[col] <- paste0("Libellé strate ", num)
    }
  }

  names(df) <- ifelse(names(df) %in% names(rename_map),
                      rename_map[names(df)],
                      names(df))
  df
}


############################################################
# FONCTION INTERNE .export_pdf


# Génération du PDF à partir d'un data.frame
.export_pdf <- function(df, nom_pdf, chemin) {

  df_pdf   <- .rename_pdf(df)
  rmd_temp <- tempfile(fileext = ".Rmd")

  writeLines(c(
    "---",
    "output:",
    "  pdf_document:",
    "    latex_engine: xelatex",
    "geometry: margin=2cm",
    "---",
    "",
    "```{r echo=FALSE, message=FALSE, warning=FALSE}",
    "library(knitr)",
    "kable(df_pdf, align = 'c', row.names = FALSE)",
    "```"
  ), rmd_temp)

  rmarkdown::render(
    input       = rmd_temp,
    output_file = nom_pdf,
    output_dir  = chemin,
    envir       = environment(),
    quiet       = TRUE
  )

  file.remove(rmd_temp)
}


