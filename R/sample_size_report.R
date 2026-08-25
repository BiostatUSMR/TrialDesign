#' Generate a Word report for a sample size calculation
#'
#' @description
#' Generates a Word (\code{.docx}) report containing the results of a sample
#' size calculation performed using the sample size functions provided by the
#' package.
#'
#' The report includes:
#' \itemize{
#'   \item a title;
#'   \item general study information;
#'   \item a table summarizing the sample size calculation results.
#' }
#'
#' @param result A data frame returned by one of the sample size calculation
#' functions, such as \code{ss_mean_sup()}, \code{ss_mean_ni()}, \code{ss_prop_sup()}, or \code{ss_prop_ni()}.
#' @param file Character string. Name of the output Word file. Defaults to \code{"sample_size_report.docx"}.
#' @param nom_etude Character string. Study name.
#' @param investigateur Character string. Name of the principal investigator.
#' @param methodologiste Character string. Name of the methodologist.
#' @param biostatisticien Character string. Name of the biostatistician.
#' @param font Character string. Font family used in the report. Defaults to \code{"Arial"}.
#' @param size Numeric. Font size used in the report. Defaults to \code{11}.
#' @param col_widths Numeric vector specifying the widths of the columns in the
#' results table. Column names must correspond to the names of the columns in
#' \code{result}, and values specify their widths in the unit defined by \code{unit}. Defaults to \code{NULL}.
#' @param min_width Numeric. Minimum width applied to each column. Expressed in the
#' unit defined by \code{unit}. Defaults to \code{1}.
#' @param unit Character string. Unit used for \code{col_widths}, \code{min_width},
#' and \code{margin}. Either \code{"cm"} (the default) or \code{"in"}.
#' @param margin Numeric. Page margin applied to the document. Can be a single value
#' applied to all margins or a numeric vector specifying the top, bottom, left, and
#' right margins. Expressed in the unit defined by \code{unit}. Defaults to \code{1.5}.
#'
#' @return The path to the generated Word file, returned invisibly.
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
    size = 11,
    col_widths = NULL,
    min_width = 1,
    unit = c("cm", "in"),
    margin = 1.5
) {

  unit <- match.arg(unit)
  cm_to_in <- function(x) x / 2.54

  #--------------------------------------------------
  # Verifications
  #--------------------------------------------------
  if (!inherits(result, "data.frame")) stop("'result' doit etre un data.frame.")

  type <- attr(result, "ssdesignr_type")
  if (is.null(type)) stop("'result' ne provient pas d'une fonction ssdesignr.")

  is_cluster <- isTRUE(attr(result, "ssdesignr_cluster"))

  titre <- switch(
    type,
    mean_sup = "Calcul du NSN - Comparaison de deux moyennes (superiorite)",
    mean_ni  = "Calcul du NSN - Comparaison de deux moyennes (non-inferiorite)",
    prop_sup = "Calcul du NSN - Comparaison de deux proportions (superiorite)",
    prop_ni  = "Calcul du NSN - Comparaison de deux proportions (non-inferiorite)",
    phase2   = "Calcul du NSN - Essai de phase II a un bras",
    precision_prop      = "Calcul du NSN - Precision d'une proportion",
    precision_sens      = "Calcul du NSN - Precision d'une sensibilite",
    precision_spec      = "Calcul du NSN - Precision d'une specificite",
    precision_sens_spec = "Calcul du NSN - Precision sensibilite/specificite",
    "Calcul du NSN"
  )

  if (is_cluster) {
    titre <- paste(titre, "- Essai randomise en clusters")
  }

  #--------------------------------------------------
  # Date + fichier
  #--------------------------------------------------
  date_str <- format(Sys.Date(), "%Y%m%d")

  if (!grepl("\\.docx$", file)) {file <- paste0(file, "_", date_str, ".docx")}
  else {file <- sub("\\.docx$", paste0("_", date_str, ".docx"), file)}

  #--------------------------------------------------
  # Styles texte
  #--------------------------------------------------
  style_titre <- fp_text(font.family = font, font.size = size + 4, bold = TRUE)
  style_texte <- fp_text(font.family = font, font.size = size)

  #--------------------------------------------------
  # Document
  #--------------------------------------------------
    # Conversion cm -> pouces si necessaire (flextable/officer travaillent en pouces)
  if (unit == "cm") {
    if (!is.null(col_widths)) col_widths <- cm_to_in(col_widths)
    min_width <- cm_to_in(min_width)
    margin    <- cm_to_in(margin)
  }

  doc <- read_docx()

  # Marges resserrees (deja en pouces a ce stade), reutilisees pour le calcul de largeur utile
  custom_margins <- officer::page_mar(
    top = margin, bottom = margin,
    left = margin, right = margin,
    gutter = 0, header = 0.3, footer = 0.3
  )

  doc <- officer::body_set_default_section(
    doc,
    officer::prop_section(
      page_size    = officer::page_size(orient = "landscape"),
      page_margins = custom_margins,
      type         = "continuous"
    )
  )

   # Titre
  doc <- body_add_fpar(doc, fpar(ftext(titre, prop = style_titre)))
  for (i in 1:3) {doc <- body_add_par(doc, "")}

  # Infos etude
  if (!is.null(nom_etude))       {doc <- body_add_fpar(doc,fpar(ftext(paste("etude :", nom_etude), prop = style_texte)))
  doc <- body_add_par(doc, "")}
  if (!is.null(investigateur))   {doc <- body_add_fpar(doc,fpar(ftext(paste("Investigateur :", investigateur), prop = style_texte)))
  doc <- body_add_par(doc, "")}
  if (!is.null(methodologiste))  {doc <- body_add_fpar(doc,fpar(ftext(paste("Methodologiste :", methodologiste), prop = style_texte)))
  doc <- body_add_par(doc, "")}
  if (!is.null(biostatisticien)) {doc <- body_add_fpar(doc,fpar(ftext(paste("Biostatisticien :", biostatisticien), prop = style_texte)))
  doc <- body_add_par(doc, "")}

  # Tableau
  doc <- body_add_par(doc, "")
  ft <- flextable(result)
  ft <- theme_booktabs(ft)
  ft <- fontsize(ft, size = size, part = "all")
  ft <- font(ft, fontname = font, part = "all")

  # 1. Largeurs calculees automatiquement sur le corps de tableau
  largeurs <- flextable::dim_pretty(ft, part = "body")$widths
  names(largeurs) <- ft$col_keys   # <-- correctif : on nomme le vecteur

  # 2. Plancher de largeur
  largeurs <- pmax(largeurs, min_width)

  # 3. Surcharge manuelle par l'utilisateur
  if (!is.null(col_widths)) {

    noms_inconnus <- setdiff(names(col_widths), names(largeurs))
    if (length(noms_inconnus) > 0) {
      warning("Colonnes inconnues dans 'col_widths' et ignorees : ",
              paste(noms_inconnus, collapse = ", "))
    }

    noms_valides <- intersect(names(col_widths), names(largeurs))
    largeurs[noms_valides] <- col_widths[noms_valides]
  }

  # 4. Normalisation a la largeur utile de la page (memes marges que le document)
  page_dims    <- officer::page_size(orient = "landscape")
  usable_width <- page_dims$width - custom_margins$left - custom_margins$right

  if (sum(largeurs) > usable_width) {
    ratio <- usable_width / sum(largeurs)
    largeurs <- largeurs * ratio
    message("Largeurs totales trop importantes pour la page : ",
            "redimensionnement proportionnel applique (ratio = ", round(ratio, 2), ").")
  }

  # 5. Application
  ft <- flextable::width(ft, width = largeurs)
  ft <- flextable::set_table_properties(ft, layout = "fixed")


  ft <- flextable::style(ft, part = "header", pr_t = officer::fp_text(font.family = font, font.size = size, bold = TRUE))
  ft <- flextable::valign(ft, valign = "center", part = "header")

  doc <- flextable::body_add_flextable(doc, ft)


  #--------------------------------------------------
  # Export
  #--------------------------------------------------
  out_dir <- getwd()
  full_path <- file.path(out_dir, file)
  print(doc, target = full_path)
  message("Rapport Word genere sous : ", out_dir)
  invisible(full_path)
}
