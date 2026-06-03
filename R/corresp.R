#' FONCTION corresp()
#'
#' @description
#' G\u00e9n\u00e8re la liste de correspondance bo\u00eetes\u2013traitements selon le circuit
#' d\u00e9fini dans l'objet \code{essai} cr\u00e9\u00e9 par \code{init_essai()}, exporte
#' automatiquement deux fichiers (donn\u00e9es + PDF), et retourne le data.frame
#' g\u00e9n\u00e9r\u00e9 pour inspection visuelle.
#'
#' @param essai Liste. Objet cr\u00e9\u00e9 par \code{init_essai()}.
#'   Exemple : \code{essai <- init_essai(...)}.
#' @param mini Entier. Premier num\u00e9ro de bo\u00eete.
#' @param maxi Entier. Dernier num\u00e9ro de bo\u00eete.
#' @param seed Entier. Graine al\u00e9atoire pour la reproductibilit\u00e9.
#' @param statut Caract\u00e8re. Statut de la liste g\u00e9n\u00e9r\u00e9e : \code{"FINALE"} ou
#'   \code{"FICTIVE"}.
#' @param version Caract\u00e8re. Version de la liste g\u00e9n\u00e9r\u00e9e. Ex : \code{"v01"}.
#' @param boi_label Caract\u00e8re. Libell\u00e9 personnalis\u00e9 pour les bo\u00eetes.
#'   Si NULL, \code{"Bo\u00eete de traitement n\u00b0xxx"} est utilis\u00e9.
#'   Utilis\u00e9 uniquement en circuit REDCap.
#' @param col_widths Vecteur de caract\u00e8res. Largeurs des colonnes du tableau PDF.
#'   Ex : \code{c("2cm", "3cm", "4cm")}. Si NULL, largeurs automatiques.
#' @param chemin Caract\u00e8re. Chemin vers le r\u00e9pertoire de sortie.
#'   Si NULL, r\u00e9pertoire de travail courant.
#'
#' @return Le data.frame de la liste de correspondance (retourn\u00e9 visiblement
#'   pour inspection). Les fichiers sont export\u00e9s en parall\u00e8le.
#'
#' @examples
#' \dontrun{
#' essai <- init_essai(
#'   nom_etude = "ESSAI_ABC", circuit = "ennov", k = 2,
#'   block_sizes = c(4), nb_block = c(10),
#'   arm_label = c("Placebo", "Traitement"), arm_code = c(0, 1)
#' )
#'
#' # Inspection du data.frame + export des fichiers
#' df <- corresp(essai, mini = 1, maxi = 50, seed = 42,
#'               statut = "FICTIVE", version = "v01")
#' head(df)
#'
#' # Deux essais en parall\u00e8le sans conflit
#' df_A <- corresp(essai_A, mini = 1, maxi = 100, seed = 42, ...)
#' df_B <- corresp(essai_B, mini = 1, maxi = 100, seed = 42, ...)
#' }
#'
#' @export

corresp <- function(essai, mini, maxi, seed, statut, version,
                    boi_label  = NULL,
                    col_widths = NULL,
                    chemin     = NULL) {

  # --- V\u00e9rification de l'objet essai ---
  if (!is.list(essai) || is.null(essai$circuit)) {
    stop(
      "'essai' doit \u00eatre un objet cr\u00e9\u00e9 par init_essai().\n",
      "Exemple : essai <- init_essai(...) puis corresp(essai, mini = 1, maxi = 50, ...)"
    )
  }

  circuit   <- essai$circuit
  nom_etude <- essai$nom_etude

  # --- Valeur par d\u00e9faut ---
  if (is.null(chemin)) chemin <- getwd()

  # --- V\u00e9rifications ---
  if (!is.numeric(mini) || mini != round(mini) || mini <= 0)
    stop("'mini' doit \u00eatre un entier strictement positif.")

  if (!is.numeric(maxi) || maxi != round(maxi) || maxi <= 0)
    stop("'maxi' doit \u00eatre un entier strictement positif.")

  if (maxi <= mini)
    stop("'maxi' doit \u00eatre strictement sup\u00e9rieur \u00e0 'mini'.")

  if (!is.numeric(seed) || seed != round(seed))
    stop("'seed' doit \u00eatre un entier.")

  if (!statut %in% c("FINALE", "FICTIVE"))
    stop("'statut' doit \u00eatre 'FINALE' ou 'FICTIVE'.")

  if (!dir.exists(chemin))
    stop("Le chemin sp\u00e9cifi\u00e9 n'existe pas.")

  if (!is.null(boi_label) && !is.character(boi_label))
    stop("'boi_label' doit \u00eatre une cha\u00eene de caract\u00e8res.")

  if (!is.null(col_widths) && !is.character(col_widths))
    stop("'col_widths' doit \u00eatre un vecteur de caract\u00e8res. Ex : c('2cm', '3cm').")

  # --- G\u00e9n\u00e9ration selon le circuit ---
  df <- if (circuit == "ennov") {
    .corresp_ennov(essai, mini, maxi, seed)
  } else {
    .corresp_redcap(essai, mini, maxi, seed, boi_label)
  }

  message("\u2714 Liste de correspondance g\u00e9n\u00e9r\u00e9e \u2014 ", nrow(df), " bo\u00eetes.")

  # --- Nom de base des fichiers ---
  ext      <- if (circuit == "redcap") "csv" else "txt"
  date_str <- format(Sys.Date(), "%Y%m%d")
  nom_base <- paste0(nom_etude, " - Liste de correspondance ",
                     statut, " - ", version, " - ", date_str)

  # --- Export donn\u00e9es ---
  nom_data <- file.path(chemin, paste0(nom_base, ".", ext))

  if (circuit == "redcap") {
    write.csv(df[, c("RDBOI", "RDBOI_LIB")],
              file = nom_data, row.names = FALSE)
  } else {
    write.table(df[, c("RDBOI", "RDGRP")],
                file      = nom_data,
                row.names = FALSE,
                col.names = FALSE,
                sep       = ";")
  }

  # --- Export PDF ---
  nom_pdf <- paste0(nom_base, ".pdf")
  .export_pdf(essai, df, nom_pdf, chemin,
              type_doc   = "correspondance",
              col_widths = col_widths)

  message("\u2714 Fichiers export\u00e9s :")
  message("  ", nom_data)
  message("  ", file.path(chemin, nom_pdf))

  # --- Retour visible du data.frame pour inspection ---
  return(df)
}
