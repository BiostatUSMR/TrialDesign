#' FONCTION corresp()
#'
#' @description
#' Génère la liste de correspondance boîtes–traitements selon le circuit
#' défini dans l'objet \code{essai} créé par \code{init_essai()}, exporte
#' automatiquement trois fichiers (données + PDF + XLS trié par bras),
#' et retourne le data.frame généré pour inspection visuelle.
#'
#' @param essai Liste. Objet créé par \code{init_essai()}.
#'   Exemple : \code{essai <- init_essai(...)}.
#' @param mini Entier. Premier numéro de boîte.
#' @param maxi Entier. Dernier numéro de boîte.
#' @param seed Entier. Graine aléatoire pour la reproductibilité.
#' @param statut Caractère. Statut de la liste générée : \code{"FINALE"} ou
#'   \code{"FICTIVE"}.
#' @param version Caractère. Version de la liste générée. Ex : \code{"v01"}.
#' @param boi_label Caractère. Libellé personnalisé pour les boîtes.
#'   Si NULL, \code{"Boîte de traitement n°xxx"} est utilisé.
#'   Utilisé uniquement en circuit REDCap.
#' @param col_widths Vecteur de caractères. Largeurs des colonnes du tableau PDF.
#'   Ex : \code{c("2cm", "3cm", "4cm")}. Si NULL, largeurs automatiques.
#' @param chemin Caractère. Chemin vers le répertoire de sortie.
#'   Si NULL, répertoire de travail courant.
#'
#' @return Le data.frame de la liste de correspondance (retourné visiblement
#'   pour inspection). Les fichiers suivants sont exportés en parallèle :
#'   \itemize{
#'     \item Fichier de données (.txt pour Ennov, .csv pour REDCap)
#'     \item Fichier PDF avec page de garde institutionnelle
#'     \item Fichier XLS avec une feuille par bras de traitement,
#'       boîtes triées par numéro croissant
#'   }
#'
#' @examples
#' \dontrun{
#' essai <- init_essai(
#'   nom_etude = "ESSAI_ABC", circuit = "ennov", k = 2,
#'   block_sizes = c(4), nb_block = c(10),
#'   arm_label = c("Placebo", "Traitement"), arm_code = c(0, 1)
#' )
#' df <- corresp(essai, mini = 1, maxi = 50, seed = 42,
#'               statut = "FICTIVE", version = "v01")
#' head(df)
#' }
#'
#' @importFrom utils write.csv write.table
#' @importFrom rmarkdown render
#'
#' @export

corresp <- function(essai, mini, maxi, seed, statut, version,
                    boi_label  = NULL,
                    col_widths = NULL,
                    chemin     = NULL) {

  # --- Vérification de l'objet essai ---
  if (!is.list(essai) || is.null(essai$circuit)) {
    stop(
      "'essai' doit être un objet créé par init_essai().\n",
      "Exemple : essai <- init_essai(...) puis corresp(essai, mini = 1, maxi = 50, ...)"
    )
  }

  circuit   <- essai$circuit
  nom_etude <- essai$nom_etude
  arm_label <- essai$arm_label
  arm_code  <- essai$arm_code

  if (is.null(chemin)) chemin <- getwd()

  # --- Vérifications ---
  if (!is.numeric(mini) || mini != round(mini) || mini <= 0)
    stop("'mini' doit être un entier strictement positif.")

  if (!is.numeric(maxi) || maxi != round(maxi) || maxi <= 0)
    stop("'maxi' doit être un entier strictement positif.")

  if (maxi <= mini)
    stop("'maxi' doit être strictement supérieur à 'mini'.")

  if (!is.numeric(seed) || seed != round(seed))
    stop("'seed' doit être un entier.")

  if (!statut %in% c("FINALE", "FICTIVE"))
    stop("'statut' doit être 'FINALE' ou 'FICTIVE'.")

  if (!dir.exists(chemin))
    stop("Le chemin spécifié n'existe pas.")

  if (!is.null(boi_label) && !is.character(boi_label))
    stop("'boi_label' doit être une chaîne de caractères.")

  if (!is.null(col_widths) && !is.character(col_widths))
    stop("'col_widths' doit être un vecteur de caractères. Ex : c('2cm', '3cm').")

  # --- Génération selon le circuit ---
  df <- if (circuit == "ennov") {
    .corresp_ennov(essai, mini, maxi, seed)
  } else {
    .corresp_redcap(essai, mini, maxi, seed, boi_label)
  }

  message("\u2714 Liste de correspondance générée \u2014 ", nrow(df), " boîtes.")

  # --- Nom de base des fichiers ---
  ext      <- if (circuit == "redcap") "csv" else "txt"
  date_str <- format(Sys.Date(), "%Y%m%d")
  nom_base <- paste0(nom_etude, " - Liste de correspondance ",
                     statut, " - ", version, " - ", date_str)

  # --- Export données (TXT ou CSV) ---
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

  # --- Export XLS trié par bras de traitement ---
  nom_xls <- file.path(chemin, paste0(nom_base, ".xlsx"))
  .export_corresp_xls(df, nom_xls, arm_label, arm_code)

  message("\u2714 Fichiers exportés :")
  message("  ", nom_data)
  message("  ", file.path(chemin, nom_pdf))
  message("  ", nom_xls)

  return(df)
}
