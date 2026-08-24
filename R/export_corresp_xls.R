# ============================================================
# FONCTION INTERNE  .export_corresp_xls()
#
# Genere un fichier Excel avec une feuille par bras de
# traitement. Dans chaque feuille, les boites sont triees
# par numero croissant.
# ============================================================

.export_corresp_xls <- function(df, nom_xls, arm_label, arm_code) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning(
      "Le package 'openxlsx' n'est pas installe. ",
      "Le fichier XLS n'a pas ete genere. ",
      "Installez-le avec : install.packages('openxlsx')"
    )
    return(invisible(NULL))
  }

  cols_xlsx <- intersect(c("rdboi", "rdgrp", "rdgrp_lib"), names(df))
  df_export <- df[, cols_xlsx, drop = FALSE]

    # En-tetes de colonnes personnalises
  names(df_export) <- c("Numero de boite", "Code du traitement", "Libelle de traitement")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "Liste de correspondance")
  openxlsx::writeData(
    wb,
    sheet    = "Liste de correspondance",
    x        = df_export,
    startRow = 1,
    startCol = 1
  )

  # Bordures sur les cellules remplies uniquement (en-tete + donnees),
  # pas sur le reste de la feuille
  n_lignes <- nrow(df_export)
  n_cols   <- ncol(df_export)

  style_bordure <- openxlsx::createStyle(border = "TopBottomLeftRight")

  openxlsx::addStyle(
    wb, sheet = "Liste de correspondance",
    style     = style_bordure,
    rows      = seq_len(n_lignes + 1),   # +1 pour inclure la ligne d'en-tete
    cols      = seq_len(n_cols),
    gridExpand = TRUE
  )

  # Largeur des colonnes adaptee automatiquement au contenu
  openxlsx::setColWidths(
    wb, sheet = "Liste de correspondance",
    cols   = seq_len(n_cols),
    widths = "auto"
  )

  openxlsx::saveWorkbook(wb, file = nom_xls, overwrite = TRUE)
}
