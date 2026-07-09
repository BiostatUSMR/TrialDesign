############################################################
# FONCTION INTERNE .rename_pdf

# Renommage des colonnes techniques en libelles lisibles pour le PDF
.rename_pdf <- function(df) {

  # Echappement des caracteres LaTeX speciaux dans le CONTENU des colonnes
  # texte (ex: le degre "n\u00b0" dans RDBOI_LIB), pour un rendu PDF correct
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- .escape_latex(df[[col]])
    }
  }

  rename_map <- c(
    rdnum     = "Numero de randomisation",
    rdgrp     = "Code Traitement",
    rdgrp_lib = "Libelle Traitement",
    rdstr     = "Code strate",
    rdstr_lib = "Libelle strate",
    rdboi     = "Numero de boite de traitement",
    rdboi_lib = "Libelle de boite de traitement"
  )

  # Colonnes rdstr1, rdstr_lib1, rdstr2, rdstr_lib2, etc. (circuit REDCap)
  for (col in names(df)) {
    if (grepl("^rdstr(\\d+)$", col)) {
      num <- sub("^rdstr(\\d+)$", "\\1", col)
      rename_map[col] <- paste0("Code strate ", num)
    }
    if (grepl("^rdstr_lib(\\d+)$", col)) {
      num <- sub("^rdstr_lib(\\d+)$", "\\1", col)
      rename_map[col] <- paste0("Libelle strate ", num)
    }
  }

  names(df) <- ifelse(names(df) %in% names(rename_map),
                      rename_map[names(df)],
                      names(df))
  df
}
