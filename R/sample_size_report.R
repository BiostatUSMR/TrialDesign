#' FONCTION ss_report()
#'
#' @description
#' Genere un rapport Word (.docx) contenant le resultat d'un calcul
#' du nombre de sujets necessaires realise avec les fonctions du package.
#'
#' Le document contient :
#' \itemize{
#'   \item un titre ;
#'   \item les informations generales de l'etude ;
#'   \item le tableau des resultats.
#' }
#'
#' @param result data.frame retourne par ss_mean_sup(), ss_mean_ni(),
#' ss_prop_sup() ou ss_prop_ni().
#' @param file Caractere. Nom du fichier Word (sans chemin). Par defaut "sample_size_report.docx".
#' @param nom_etude Caractere. Nom de l'etude.
#' @param investigateur Caractere. Nom de l'investigateur.
#' @param methodologiste Caractere. Nom du methodologiste.
#' @param biostatisticien Caractere. Nom du biostatisticien.
#' @param font Caractere. Police. Par defaut "Arial".
#' @param size Caractere. Taille de police. Par defaut 11.
#' @param col_widths Numerique. Vecteur donnant la largeur de colonnes du tableau de resultats (nom de colonne = largeur). Exprime dans l'unite definie par \code{unit}. Par defaut NULL.
#' @param min_width Numerique. Largeur minimale appliquee a chaque colonne. Exprime dans l'unite definie par \code{unit}. Par defaut 1 (cm).
#' @param unit Caractere. Unite utilisee pour \code{col_widths} et \code{min_width} : \code{"cm"} (par defaut) ou \code{"in"} (pouces).
#' @param margin Numerique. Marge de page (haut, bas, gauche, droite) appliquee au document. Exprimee dans l'unite definie par \code{unit}. Par defaut 1.5 (cm).
#'
#' @return Chemin du fichier genere (invisiblement).
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
