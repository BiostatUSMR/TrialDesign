#' Generate an HTML report for a sample size calculation
#'
#' @description
#' Generates an HTML report containing the results of a sample size calculation
#' performed using one of the sample size functions provided by the package.
#'
#' The report includes:
#' \itemize{
#'   \item a title describing the type of sample size calculation;
#'   \item general study information;
#'   \item a table summarizing the sample size calculation results.
#' }
#'
#' @param result A data frame returned by one of the sample size calculation
#' functions of the package, such as \code{ss_mean_sup()},
#' \code{ss_mean_ni()}, \code{ss_prop_sup()}, \code{ss_prop_ni()},
#' \code{ss_phase2()}, or \code{ss_precision()}.
#'
#' The result may also be returned by \code{ss_cluster()}.
#' @param file Character string. Name of the output HTML file. Defaults to
#' \code{"sample_size_report.html"}. The current date is automatically appended to the file name.
#' @param nom_etude Character string. Study name. Defaults to \code{NULL}.
#' @param investigateur Character string. Name of the principal investigator. Defaults to \code{NULL}.
#' @param methodologiste Character string. Name of the methodologist. Defaults to \code{NULL}.
#' @param biostatisticien Character string. Name of the biostatistician. Defaults to \code{NULL}.
#' @param font Character string. Font family used in the report. Defaults to \code{"Arial"}.
#' @param size Numeric. Base font size used in the report, in pixels. Defaults to \code{14}.
#'
#' @return The path to the generated HTML file, returned invisibly.
#'
#' @details
#' The HTML report is self-contained and can be opened directly in a web
#' browser. When the results table is wider than the available display area,
#' a horizontal scrollbar is automatically provided.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate the sample size for a superiority trial comparing two means
#' res <- ss_mean_sup(
#'   mu1   = 10,
#'   mu2   = 12,
#'   sd    = 4,
#'   alpha = 0.05,
#'   power = 0.80,
#'   sided = 2
#' )
#'
#' # Adjust the sample size for a cluster randomized design
#' res_cluster <- ss_cluster(
#'   n_ind  = res,
#'   schema = "crt",
#'   m      = 25,
#'   icc    = 0.05
#' )
#'
#' # Generate the HTML report
#' ss_report(
#'   result = res_cluster,
#'   file = "sample_size_report.html",
#'   nom_etude = "Study name",
#'   investigateur = "Investigator name",
#'   methodologiste = "Methodologist name",
#'   biostatisticien = "Biostatistician name"
#' )
#' }
#'

ss_report <- function(
    result,
    file = "sample_size_report.html",
    nom_etude = NULL,
    investigateur = NULL,
    methodologiste = NULL,
    biostatisticien = NULL,
    font = "Arial",
    size = 11
) {

  #--------------------------------------------------
  # Verifications
  #--------------------------------------------------

  if (!inherits(result, "data.frame")) {
    stop("'result' doit \u00EAtre un data.frame.")
  }

  type <- attr(result, "ssdesignr_type")

  if (is.null(type)) {
    stop("'result' ne provient pas d'une fonction de calcul de taille d'\u00E9chantillon de TrialDesign.")
  }

  is_cluster <- isTRUE(
    attr(result, "ssdesignr_cluster")
  )

  #--------------------------------------------------
  # Titre
  #--------------------------------------------------

  titre <- switch(
    type,
    mean_sup = "Calcul du NSN - Comparaison de deux moyennes (sup\u00E9riorit\u00E9)",
    mean_ni  = "Calcul du NSN - Comparaison de deux moyennes (non-inf\u00E9riorit\u00E9)",
    prop_sup = "Calcul du NSN - Comparaison de deux proportions (sup\u00E9riorit\u00E9)",
    prop_ni  = "Calcul du NSN - Comparaison de deux proportions (non-inf\u00E9riorit\u00E9)",
    phase2   = "Calcul du NSN - Essai de phase II mono-bras",
    precision_prop      = "Calcul du NSN - Pr\u00E9cision d'une proportion",
    precision_sens      = "Calcul du NSN - Pr\u00E9cision d'une sensibilit\u00E9",
    precision_spec      = "Calcul du NSN - Pr\u00E9cision d'une sp\u00E9cificit\u00E9",
    precision_sens_spec = "Calcul du NSN - Pr\u00E9cision sensibilit\u00E9/sp\u00E9cificit\u00E9",
    "Calcul du NSN"
  )

  if (is_cluster) {titre <- paste(titre," - Essai randomis\u00E9 en clusters")}

  #--------------------------------------------------
  # Nom du fichier
  #--------------------------------------------------

  date_str <- format(Sys.Date(), "%Y%m%d")
  if (!grepl("\\.html$", file, ignore.case = TRUE)) {file <- paste0(file, "_", date_str, ".html")}
  else {file <- sub("\\.html$", paste0("_", date_str, ".html"), file, ignore.case = TRUE)}

  #--------------------------------------------------
  # Tableau
  #--------------------------------------------------

  ft <- flextable::flextable(result)

  # Identification des colonnes d'effectifs
  noms_result <- names(result)

    # Cas standard :n1 / n2 / n_total / n1_pdv / n2_pdv / n_total_pdv
    is_standard <- all(c("n1", "n2", "n_total", "n1_pdv", "n2_pdv", "n_total_pdv") %in% noms_result)

    # Cas cluster : n_total / n_cluster / n_total_pdv / n_cluster_pdv
    is_cluster_table <- all(c("n_total", "n_cluster", "n_total_pdv", "n_cluster_pdv") %in% noms_result)

    # Cas precision : sensibilité + spécificité
    is_precision_sens_spec <- all(c("n_sens", "n_sens_ajuste", "n_spec", "n_spec_ajuste") %in% noms_result)

    # Cas precision : une seule mesure
    is_precision_single <- (sum(c("n_prop", "n_sens", "n_spec") %in% noms_result) == 1) &&
      (sum(c("n_prop_ajuste", "n_sens_ajuste", "n_spec_ajuste") %in% noms_result) == 1)

  # Cas standard
  if (is_standard) {

    # En-tête à deux niveaux
    col_n1     <- which(noms_result == "n1")
    col_n1_pdv <- which(noms_result == "n1_pdv")

    # Niveau : Hypothèses / Sans d.m / Avec d.m
    ft <- flextable::add_header_row(ft, values = c("Hypoth\u00E8ses", "Sans d.m", "Avec d.m"),
                                    colwidths = c(col_n1 - 1, 3, 3))

    # Fond
    ft <- flextable::bg(ft, j = which(noms_result %in% c("n1", "n2", "n_total")), bg = "#F5F5F5", part = "all")
    ft <- flextable::bg(ft, j = which(noms_result %in% c("n1_pdv", "n2_pdv", "n_total_pdv")), bg = "#EAEAEA", part = "all")

    # Séparateurs verticaux
    ft <- flextable::vline(ft, j = col_n1,     border = officer::fp_border(width = 1), part = "all")
    ft <- flextable::vline(ft, j = col_n1_pdv, border = officer::fp_border(width = 1), part = "all")

  # Cas cluster
  } else if (is_cluster_table) {

    col_n_total     <- which(noms_result == "n_total")
    col_n_total_pdv <- which(noms_result == "n_total_pdv")

    # Niveau : Hypothèses / Sans d.m / Avec d.m
    ft <- flextable::add_header_row(ft, values = c("Hypoth\u00E8ses", "Sans d.m", "Avec d.m", ""),
                                    colwidths = c(col_n_total - 1, 2, 2, ncol(result) - col_n_total_pdv - 1))
    # Fond
    ft <- flextable::bg(ft, j = which(noms_result %in% c("n_total", "n_cluster")), bg = "#F5F5F5", part = "all")
    ft <- flextable::bg(ft, j = which(noms_result %in% c("n_total_pdv", "n_cluster_pdv")), bg = "#EAEAEA", part = "all")

    # Séparateurs
    ft <- flextable::vline(ft, j = col_n_total, border = officer::fp_border(width = 1), part = "all")
    ft <- flextable::vline(ft, j = col_n_total_pdv, border = officer::fp_border(width = 1), part = "all")

# Cas précision sensibilité/spécificité
  } else if (is_precision_sens_spec) {

    # Identification des colonnes
    col_n_sens        <- which(noms_result == "n_sens")
    col_n_sens_ajuste <- which(noms_result == "n_sens_ajuste")
    col_n_spec        <- which(noms_result == "n_spec")
    col_n_spec_ajuste <- which(noms_result == "n_spec_ajuste")

    # Nombre de colonnes avant les effectifs
    n_cols_avant <- col_n_sens - 1

    # Niveau : Hypothèses / Sensibilité / Spécificité // Sans d.m / Avec d.m
    ft <- flextable::add_header_row(ft, values = c("", "Sans d.m", "Avec d.m", "Sans d.m", "Avec d.m"),
                                    colwidths = c(n_cols_avant, 1, 1, 1, 1))
    ft <- flextable::add_header_row(ft, values = c("Hypoth\u00E8ses", "Sensibilit\u00E9", "Sp\u00E9cificit\u00E9"),
                                    colwidths = c(n_cols_avant, 2, 2))

    # Fond
    ft <- flextable::bg(ft, j = c(col_n_sens, col_n_spec), bg = "#F5F5F5", part = "all")
    ft <- flextable::bg(ft, j = c( col_n_sens_ajuste, col_n_spec_ajuste), bg = "#EAEAEA", part = "all")

    # Séparateurs verticaux
    ft <- flextable::vline(ft, j = col_n_sens, border = officer::fp_border(width = 1), part = "all")
    ft <- flextable::vline(ft, j = col_n_spec, border = officer::fp_border(width = 1), part = "all")

# Cas précision simple
  } else if (is_precision_single) {

    # Identification des colonnes
    col_sans <- which(noms_result %in% c("n_prop","n_sens","n_spec"))
    col_avec <- which(noms_result %in% c("n_prop_ajuste", "n_sens_ajuste", "n_spec_ajuste"))

    # Niveau : Hypothèses / Sans d.m / Avec d.m
    ft <- flextable::add_header_row(ft, values = c("Hypoth\u00E8ses", "Sans d.m", "Avec d.m"),
                                    colwidths = c(col_sans - 1, 1, 1))
    # Fond
    ft <- flextable::bg(ft, j = col_sans, bg = "#F5F5F5", part = "all")
    ft <- flextable::bg(ft, j = col_avec, bg = "#EAEAEA", part = "all")

    # Séparateurs verticaux
    ft <- flextable::vline(ft, j = col_sans, border = officer::fp_border(width = 1), part = "all")
    ft <- flextable::vline(ft, j = col_avec, border = officer::fp_border(width = 1), part = "all")
  }

  #--------------------------------------------------
  # Style général
  #--------------------------------------------------
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft, size = size, part = "all")
  ft <- flextable::font(ft, fontname = font, part = "all")
  ft <- flextable::bold(ft, bold = TRUE, part = "header")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::valign(ft, valign = "center", part = "header")

  # Ligne horizontale sous les titres des groupes
  if (is_standard || is_cluster_table || is_precision_single || is_precision_sens_spec) {
    ft <- flextable::hline(ft, i = 1, border = officer::fp_border(width = 1), part = "header")
  }

  # Ajustement automatique des largeurs
  ft <- flextable::autofit(ft)

  #--------------------------------------------------
  # Informations de l'etude
  #--------------------------------------------------

  infos <- list()
  if (!is.null(nom_etude))
  {infos <- c(infos,list(htmltools::tags$p(htmltools::tags$b("\u00C9tude : : "), nom_etude)))}
  if (!is.null(investigateur))
  {infos <- c(infos,list(htmltools::tags$p(htmltools::tags$b("Investigateur : "),investigateur)))}
  if (!is.null(methodologiste))
  {infos <- c(infos,list(htmltools::tags$p(htmltools::tags$b("M\u00E9thodologiste : "),methodologiste)))}
  if (!is.null(biostatisticien))
  {infos <- c(infos,list(htmltools::tags$p(htmltools::tags$b("Biostatisticien : "),biostatisticien)))}

  #--------------------------------------------------
  # CSS
  #--------------------------------------------------

  css <- htmltools::tags$style(
    htmltools::HTML(paste0("
    body {font-family: ", font, "; font-size: ", size, "pt; margin: 40px;}
    h1 {font-size: ", size + 6, "pt; margin-bottom: 30px;}
    .study-info {margin-bottom: 30px;}
    .study-info p {margin: 5px 0;}
    .table-container {width: 100%; overflow-x: auto; margin-top: 25px;}
    .table-container table {width: max-content; min-width: 100%;}")))

  #--------------------------------------------------
  # Document HTML
  #--------------------------------------------------

  contenu <- htmltools::tagList(

    htmltools::tags$head(htmltools::tags$meta(charset = "UTF-8"), css),
    htmltools::tags$body(
      htmltools::tags$h1(titre),
      htmltools::tags$div(class = "study-info", htmltools::tagList(infos)),
      htmltools::tags$hr(),
      htmltools::tags$div(class = "table-container", flextable::htmltools_value(ft))))

  #--------------------------------------------------
  # Export
  #--------------------------------------------------

  full_path <- file.path(getwd(), file)
  htmltools::save_html(html = contenu, file = full_path)
  message("\u2714 Rapport HTML g\u00E9n\u00E9r\u00E9 sous : ", full_path)
  invisible(full_path)

}

