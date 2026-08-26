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
#' The results table is displayed in a horizontally scrollable container when
#' necessary, allowing wide tables to be viewed without manual specification of
#' column widths.
#'
#' @param result A data frame returned by one of the sample size calculation
#' functions of the package, such as \code{ss_mean_sup()},
#' \code{ss_mean_ni()}, \code{ss_prop_sup()}, \code{ss_prop_ni()},
#' \code{sample_size_phase2()}, or \code{sample_size_precision()}.
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
    stop("'result' doit etre un data.frame.")
  }

  type <- attr(result, "ssdesignr_type")

  if (is.null(type)) {
    stop("'result' ne provient pas d'une fonction de calcul de taille d'echantillon de TrialDesign.")
  }

  is_cluster <- isTRUE(
    attr(result, "ssdesignr_cluster")
  )

  #--------------------------------------------------
  # Titre
  #--------------------------------------------------

  titre <- switch(
    type,
    mean_sup = "Calcul du NSN - Comparaison de deux moyennes (superiorite)",
    mean_ni  = "Calcul du NSN - Comparaison de deux moyennes (non-inferiorite)",
    prop_sup = "Calcul du NSN - Comparaison de deux proportions (superiorite)",
    prop_ni  = "Calcul du NSN - Comparaison de deux proportions (non-inferiorite)",
    phase2   = "Calcul du NSN - Essai de phase II mono-bras",
    precision_prop      = "Calcul du NSN - Precision d'une proportion",
    precision_sens      = "Calcul du NSN - Precision d'une sensibilite",
    precision_spec      = "Calcul du NSN - Precision d'une specificite",
    precision_sens_spec = "Calcul du NSN - Precision sensibilite/specificite",
    "Calcul du NSN"
  )

  if (is_cluster) {titre <- paste(titre," - Essai randomise en clusters")}

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
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft, size = size, part = "all")
  ft <- flextable::font(ft, fontname = font, part = "all")
  ft <- flextable::bold(ft, bold = TRUE, part = "header")
  ft <- flextable::valign(ft, valign = "center", part = "header")

  # Largeur naturelle selon le contenu
  ft <- flextable::autofit(ft)

  #--------------------------------------------------
  # Informations de l'etude
  #--------------------------------------------------

  infos <- list()
  if (!is.null(nom_etude))
  {infos <- c(infos,list(htmltools::tags$p(htmltools::tags$b("Etude : "), nom_etude)))}
  if (!is.null(investigateur))
  {infos <- c(infos,list(htmltools::tags$p(htmltools::tags$b("Investigateur : "),investigateur)))}
  if (!is.null(methodologiste))
  {infos <- c(infos,list(htmltools::tags$p(htmltools::tags$b("Methodologiste : "),methodologiste)))}
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
  message("\u2714 Rapport HTML genere sous : ", full_path)
  invisible(full_path)

}

