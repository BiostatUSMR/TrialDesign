#' FONCTION corresp()
#'
#' @description
#' Genere la liste de correspondance boites-traitements selon le circuit
#' defini dans init_essai(), puis exporte automatiquement deux fichiers :
#' un fichier de donnees (.txt pour Ennov, .csv pour REDCap)
#' un fichier PDF avec les libelles lisibles et une page de garde.
#'
#' @param mini Entier. Premier numero de boite.
#' @param maxi Entier. Dernier numero de boite.
#' @param seed Entier. Graine aleatoire pour la reproductibilite.
#' @param statut Caractere. Statut de la liste generee "FINALE" ou "FICTIVE".
#' @param version Caractere. Version de la liste generee. Ex : "V01".
#' @param boi_label Caractere. Libelle personnalise pour les boites.
#'   Si NULL, "Boite de traitement n°xxx" est utilise.
#'   Utilise uniquement en circuit REDCap.
#' @param col_widths Vecteur de caracteres. Largeurs des colonnes du tableau PDF.
#'   Ex : c("2cm", "3cm", "4cm"). Si NULL, largeurs automatiques.
#'   Doit avoir autant d'elements que de colonnes dans le data.frame de sortie.
#' @param chemin Caractere. Chemin vers le repertoire de sortie.
#'   Si NULL, repertoire de travail courant.
#'
#' @return Invisible. Exporte les fichiers et affiche leur chemin.
#'
#' @examples
#' \dontrun{
#' # Sans largeurs personnalisees
#' corresp(mini = 1, maxi = 50, seed = 42,
#'         statut = "FICTIVE", version = "V01")
#'
#' # Avec largeurs personnalisees
#' corresp(mini = 1, maxi = 50, seed = 42,
#'         statut = "FICTIVE", version = "V01",
#'         col_widths = c("2cm", "3cm", "4cm"))
#'
#' # Circuit REDCap avec libelle personnalise
#' corresp(mini = 1, maxi = 50, seed = 42,
#'         statut = "FICTIVE", version = "V01",
#'         boi_label = "Kit")
#' }
#'
#' @export

corresp <- function(mini, maxi, seed, statut, version,
                    boi_label  = NULL,
                    col_widths = NULL,
                    chemin     = NULL) {

  circuit   <- .trialdesign_env$circuit
  nom_etude <- .trialdesign_env$nom_etude

  # Valeur par defaut
  if (is.null(chemin)) chemin <- getwd()

  # Verifications
  if (!is.numeric(mini) || mini != round(mini) || mini <= 0) {
    stop("'mini' doit etre un entier strictement positif.")
  }
  if (!is.numeric(maxi) || maxi != round(maxi) || maxi <= 0) {
    stop("'maxi' doit etre un entier strictement positif.")
  }
  if (maxi <= mini) {
    stop("'maxi' doit etre strictement superieur a 'mini'.")
  }
  if (!is.numeric(seed) || seed != round(seed)) {
    stop("'seed' doit etre un entier.")
  }
  if (!statut %in% c("FINALE", "FICTIVE")) {
    stop("'statut' doit etre 'FINALE' ou 'FICTIVE'.")
  }
  if (!dir.exists(chemin)) {
    stop("Le chemin specifie n'existe pas.")
  }
  if (!is.null(boi_label) && !is.character(boi_label)) {
    stop("'boi_label' doit etre une chaine de caracteres.")
  }
  if (!is.null(col_widths) && !is.character(col_widths)) {
    stop("'col_widths' doit etre un vecteur de caracteres. Ex : c('2cm', '3cm').")
  }

  # Generation selon le circuit
  df <- if (circuit == "ennov") {
    .corresp_ennov(mini, maxi, seed)
  } else {
    .corresp_redcap(mini, maxi, seed, boi_label)
  }

  message("\u2714 Liste de correspondance generee - ", nrow(df), " boites.")

  # Nom de base des fichiers
  ext      <- if (circuit == "redcap") "csv" else "txt"
  date_str <- format(Sys.Date(), "%Y%m%d")
  nom_base <- paste0(nom_etude, " - Liste de correspondance ",
                     statut, " - ", version, " - ", date_str)

  # Export donnees
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

  # Export PDF
  nom_pdf <- paste0(nom_base, ".pdf")
  .export_pdf(df, nom_pdf, chemin,
              type_doc   = "correspondance",
              col_widths = col_widths)

  message("\u2714 Fichiers exportes :")
  message("  ", nom_data)
  message("  ", file.path(chemin, nom_pdf))

  invisible(NULL)
}
