############################################################
# FONCTION INTERNE .rename_pdf

# Renommage des colonnes techniques en libellés lisibles pour le PDF
.rename_pdf <- function(df) {
  rename_map <- c(
    RDNUM     = "Num\u00e9ro de randomisation",
    RDGRP     = " Code Traitement",
    RDGRP_LIB = "Libell\u00e9 Traitement",
    RDSTR     = "Code strate",
    RDSTR_LIB = "Libell\u00e9 strate",
    RDBOI     = "Num\u00e9ro de bo\u00eete de traitement",
    RDBOI_LIB = "Libell\u00e9 de bo\u00eete de traitement"
  )

  # Colonnes RDSTR1, RDSTR_LIB1, RDSTR2, RDSTR_LIB2, etc. (circuit REDCap)
  for (col in names(df)) {
    if (grepl("^RDSTR(\\d+)$", col)) {
      num <- sub("^RDSTR(\\d+)$", "\\1", col)
      rename_map[col] <- paste0("Code strate ", num)
    }
    if (grepl("^RDSTR_LIB(\\d+)$", col)) {
      num <- sub("^RDSTR_LIB(\\d+)$", "\\1", col)
      rename_map[col] <- paste0("Libell\u00e9 strate ", num)
    }
  }

  names(df) <- ifelse(names(df) %in% names(rename_map),
                      rename_map[names(df)],
                      names(df))
  df
}
