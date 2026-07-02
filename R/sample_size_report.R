#' FONCTION ss_report()
#'
#' @description
#' Génère un rapport Word (.docx) contenant le résultat d'un calcul
#' du nombre de sujets nécessaires réalisé avec les fonctions du package.
#'
#' Le document contient :
#' \itemize{
#'   \item un titre ;
#'   \item les informations générales de l'étude ;
#'   \item le tableau des résultats.
#' }
#'
#' @param result data.frame retourné par ss_mean_sup(), ss_mean_ni(),
#' ss_prop_sup() ou ss_prop_ni().
#' @param file Nom du fichier Word (sans chemin). Par défaut "sample_size_report.docx".
#' @param nom_etude Nom de l'étude.
#' @param investigateur Nom de l'investigateur.
#' @param methodologiste Nom du méthodologiste.
#' @param biostatisticien Nom du biostatisticien.
#' @param font Police (par défaut "Arial").
#' @param size Taille de police (par défaut 11).
#'
#' @return Chemin du fichier généré (invisiblement).
#'
#' @importFrom officer read_docx body_add_par body_add_fpar fpar ftext fp_text
#' @importFrom flextable flextable theme_booktabs autofit fontsize font
#'
#' @export

ss_report <- function(
    result,
    file = "sample_size_report.docx",
    nom_etude = NULL,
    investigateur = NULL,
    methodologiste = NULL,
    biostatisticien = NULL,
    font = "Arial",
    size = 11
) {

  #--------------------------------------------------
  # Vérifications
  #--------------------------------------------------

  if (!inherits(result, "data.frame")) {
    stop("'result' doit être un data.frame.")
  }

  type <- attr(result, "ssdesignr_type")
  if (is.null(type)) {
    stop("'result' ne provient pas d'une fonction ssdesignr.")
  }

  titre <- switch(
    type,
    mean_sup = "Calcul du NSN - Deux moyennes (supériorité)",
    mean_ni  = "Calcul du NSN - Deux moyennes (non-infériorité)",
    prop_sup = "Calcul du NSN - Deux proportions (supériorité)",
    prop_ni  = "Calcul du NSN - Deux proportions (non-infériorité)",
    "Calcul du NSN"
  )

  #--------------------------------------------------
  # Date + fichier
  #--------------------------------------------------

  date_str <- format(Sys.Date(), "%Y%m%d")

  if (!grepl("\\.docx$", file)) {
    file <- paste0(file, "_", date_str, ".docx")
  } else {
    file <- sub("\\.docx$", paste0("_", date_str, ".docx"), file)
  }

  #--------------------------------------------------
  # Styles texte
  #--------------------------------------------------

  style_titre <- officer::fp_text(font.family = font, font.size = size + 4, bold = TRUE)
  style_texte <- officer::fp_text(font.family = font, font.size = size)

  #--------------------------------------------------
  # Document
  #--------------------------------------------------

  doc <- officer::read_docx()

  # Titre
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(ftext(titre, prop = style_titre))
  )

  doc <- officer::body_add_fpar(doc, fpar())

  # Infos étude
  if (!is.null(nom_etude)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(ftext(paste("Étude :", nom_etude), prop = style_texte))
    )
  }

  if (!is.null(investigateur)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(ftext(paste("Investigateur :", investigateur), prop = style_texte))
    )
  }

  if (!is.null(methodologiste)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(ftext(paste("Méthodologiste :", methodologiste), prop = style_texte))
    )
  }

  if (!is.null(biostatisticien)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(ftext(paste("Biostatisticien :", biostatisticien), prop = style_texte))
    )
  }

  doc <- officer::body_add_fpar(doc, fpar())

  #--------------------------------------------------
  # Tableau
  #--------------------------------------------------

  ft <- flextable(result)
  ft <- theme_booktabs(ft)
  ft <- autofit(ft)
  ft <- fontsize(ft, size = size, part = "all")
  ft <- font(ft, fontname = font, part = "all")

  doc <- body_add_flextable(doc, ft)

  #--------------------------------------------------
  # Export
  #--------------------------------------------------

  out_dir <- getwd()
  full_path <- file.path(out_dir, file)

  print(doc, target = full_path)

  message("✔ Rapport Word généré sous : ", out_file)

  invisible(full_path)
}
