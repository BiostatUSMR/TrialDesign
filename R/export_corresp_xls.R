# ============================================================
# FONCTION INTERNE — .export_corresp_xls()
#
# Génère un fichier Excel avec une feuille par bras de
# traitement. Dans chaque feuille, les boîtes sont triées
# par numéro croissant.
# ============================================================

.export_corresp_xls <- function(df, nom_xls, arm_label, arm_code) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning(
      "Le package 'openxlsx' n'est pas installé. ",
      "Le fichier XLS n'a pas été généré. ",
      "Installez-le avec : install.packages('openxlsx')"
    )
    return(invisible(NULL))
  }

  wb <- openxlsx::createWorkbook()

  # Style en-tête : fond bleu USMR, texte blanc, gras
  style_header <- openxlsx::createStyle(
    fontColour     = "#FFFFFF",
    fgFill         = "#1F3864",
    halign         = "CENTER",
    textDecoration = "bold",
    border         = "Bottom",
    borderColour   = "#FFFFFF"
  )

  # Style lignes paires : bleu clair
  style_even <- openxlsx::createStyle(
    fgFill = "#D6E4F7",
    halign = "CENTER"
  )

  # Style lignes impaires : blanc
  style_odd <- openxlsx::createStyle(
    fgFill = "#FFFFFF",
    halign = "CENTER"
  )

  # Une feuille par bras, triée par numéro de boîte croissant
  for (i in seq_along(arm_label)) {

    df_bras <- df[df$RDGRP == arm_code[i], ]
    df_bras <- df_bras[order(df_bras$RDBOI), ]
    row.names(df_bras) <- NULL

    # Nom de la feuille — limité à 31 caractères (contrainte Excel)
    nom_feuille <- substr(arm_label[i], 1, 31)

    openxlsx::addWorksheet(wb, sheetName = nom_feuille)

    openxlsx::writeData(
      wb,
      sheet       = nom_feuille,
      x           = df_bras,
      startRow    = 1,
      startCol    = 1,
      headerStyle = style_header,
      borders     = "all",
      borderColour = "#AAAAAA"
    )

    n_lignes <- nrow(df_bras)

    if (n_lignes > 0) {
      lignes_paires   <- seq(3, n_lignes + 1, by = 2)
      lignes_impaires <- seq(2, n_lignes + 1, by = 2)

      if (length(lignes_paires) > 0) {
        openxlsx::addStyle(wb, sheet = nom_feuille,
                           style = style_even,
                           rows  = lignes_paires,
                           cols  = seq_len(ncol(df_bras)),
                           gridExpand = TRUE)
      }
      if (length(lignes_impaires) > 0) {
        openxlsx::addStyle(wb, sheet = nom_feuille,
                           style = style_odd,
                           rows  = lignes_impaires,
                           cols  = seq_len(ncol(df_bras)),
                           gridExpand = TRUE)
      }
    }

    # Largeur automatique des colonnes
    openxlsx::setColWidths(wb, sheet = nom_feuille,
                           cols   = seq_len(ncol(df_bras)),
                           widths = "auto")
  }

  openxlsx::saveWorkbook(wb, file = nom_xls, overwrite = TRUE)
}
