
######################################################################################################################


#' FONCTION corresp()
#'
#' @description
#' Génère la liste de correspondance boîtes–traitements selon le circuit
#' défini dans init_essai(), puis exporte automatiquement deux fichiers :
#' un fichier de données (.txt pour Ennov et .csv  pour REDCap)
#' un fichier PDF avec les libellés lisibles
#'
#'
#' @param mini Entier. Premier numéro de boîte.
#' @param maxi Entier. Dernier numéro de boîte.
#' @param seed Entier. Graine aléatoire pour la reproductibilité.
#' @param statut Caractère. Satut de la liste générée "FINALE" ou "FICTIVE".
#' @param version Caractère. Version de la liste générée. Ex : "V01".
#' @param boi_label Caractère. Libellé Boîte de traitement.
#'  Si NULL, "Boîte de traitement n°xxx"
#' @param chemin Caractère.Caractère.Chemin vers le répertoire de sortie.
#'  Si NULL, répertoire de travail courant .
#'
#' @return Invisible. Exporte les fichiers et affiche leur chemin.
#'
#' @examples
#' \dontrun{
#' corresp(mini = 1, maxi = 50, seed = 42, statut = "FICTIVE", version = "v01",
#'         boi_label = "Kit")
#'
#'}
#' @export

corresp <- function(mini, maxi, seed, statut, version,
                    boi_label = NULL,
                    chemin    = NULL) {

  circuit   <- .trialdesign_env$circuit
  nom_etude <- .trialdesign_env$nom_etude

  # Valeurs par défaut
  if (is.null(chemin)) chemin <- getwd()

  # Vérifications
  if (!is.numeric(mini) || mini != round(mini) || mini <= 0) {
    stop("'mini' doit être un entier strictement positif.")
  }
  if (!is.numeric(maxi) || maxi != round(maxi) || maxi <= 0) {
    stop("'maxi' doit être un entier strictement positif.")
  }
  if (maxi <= mini) {
    stop("'maxi' doit être strictement supérieur à 'mini'.")
  }
  if (!is.numeric(seed) || seed != round(seed)) {
    stop("'seed' doit être un entier.")
  }
  if (!statut %in% c("FINALE", "FICTIVE")) {
    stop("'statut' doit être 'FINALE' ou 'FICTIVE'.")
  }
  if (!dir.exists(chemin)) {
    stop("Le chemin spécifié n'existe pas.")
  }

  if (!is.null(boi_label) && !is.character(boi_label)) {
    stop("'boi_label' doit être une chaîne de caractères.")
  }

  # Génération selon le circuit
  df <- if (circuit == "ennov") {
    .corresp_ennov(mini, maxi, seed)
  } else {
    .corresp_redcap(mini, maxi, seed, boi_label)
  }

  message("✔ Liste de correspondance generee — ", nrow(df), " boîtes.")

  # Nom de base des fichiers
  ext      <- if (circuit == "redcap") "csv" else "txt"
  date_str <- format(Sys.Date(), "%Y%m%d")
  nom_base <- paste0(nom_etude, " - Liste de correspondance ",
                     statut, " - ", version, " - ", date_str)

  # Export données
  nom_data <- file.path(chemin, paste0(nom_base, ".", ext))

  if (circuit == "redcap") {
    # CSV avec titres — uniquement rdboi et rdboi_lib
    write.csv(df[, c("RDBOI", "RDBOI_LIB")],
              file = nom_data, row.names = FALSE)
  } else {
    # TXT sans titres — uniquement RDBOI et RDGRP
    write.table(df[, c("RDBOI", "RDGRP")],
                file      = nom_data,
                row.names = FALSE,
                col.names = FALSE,
                sep       = "\t")
  }

  # Export PDF — toutes les colonnes avec libellés lisibles
  nom_pdf <- paste0(nom_base, ".pdf")
  .export_pdf(df, nom_pdf, chemin, type_doc = "correspondance")

  message("✔ Fichiers exportes :")
  message("  ", nom_data)
  message("  ", file.path(chemin, nom_pdf))


  invisible(NULL)
}
