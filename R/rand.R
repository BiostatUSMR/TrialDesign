

###########################################################################################################

#'FONCTION rand()
#'
#'
#' @description
#' Génère la liste de randomisation selon le circuit défini dans
#' init_essai(), puis exporte automatiquement deux fichiers :
#' un fichier de données (.csv pour Ennov, .txt pour REDCap)
#' un fichier PDF avec les libellés lisibles
#'
#'
#' @param seed Entier. Graine aléatoire pour la reproductibilité.
#' @param statut Caractère. Statut de la liste générée "FINALE" ou "FICTIVE".
#' @param version Caractère. Version de la liste générée. Ex : "V01".
#' @param chemin Caractère.Chemin vers le répertoire de sortie.
#'  Si NULL, répertoire de travail courant .
#'
#' @importFrom utils write.csv write.table
#' @importFrom rmarkdown render
#'
#' @return Invisible. Exporte les fichiers et affiche leur chemin.
#'
#' @examples
#' \dontrun{
#' rand(seed = 42, statut = "FICTIVE", version = "V1")
#' }
#'
#' @export


rand <- function(seed, statut, version, chemin = NULL) {

  circuit   <- .trialdesign_env$circuit
  nom_etude <- .trialdesign_env$nom_etude

  #Valeur par défaut
  if (is.null(chemin)) chemin <- getwd()


  #vérification des arguments
  if (!is.numeric(seed) || seed != round(seed)){
    stop("'seed' doit être un entier.")}

  if (!statut %in% c("FINALE", "FICTIVE")){
    stop("'statut' doit être 'FINALE' ou 'FICTIVE'.")}

  if (!dir.exists(chemin)) {
    stop("Le chemin spécifié n'existe pas.")}


  # Génération du data.frame
  df <- if (circuit == "ennov") .rand_ennov(seed) else .rand_redcap(seed)

  message("✔ Liste de randomisation generee — ", nrow(df), " sujets.")


  # Nom de base des fichiers
  ext      <- if (circuit == "redcap") "csv" else "txt"
  date_str <- format(Sys.Date(), "%Y%m%d")
  nom_base <- paste0(nom_etude, " - Liste de randomisation ",
                     statut, " - ", version, " - ", date_str)

  # Export données
  nom_data <- file.path(chemin, paste0(nom_base, ".", ext))

  if (circuit == "redcap") {
    write.csv(df, file = nom_data, row.names = FALSE)
  } else {
    cols_rdstr <- grep("^RDSTR\\d*$", names(df), value = TRUE)
    df_txt     <- df[, c(cols_rdstr, "RDNUM", "RDGRP"), drop = FALSE]
    write.table(df_txt, file = nom_data, row.names = FALSE,
                col.names = FALSE, sep = ";")
  }

  # Export PDF
  nom_pdf   <- file.path(chemin, paste0(nom_base, ".pdf"))

  .export_pdf(df, nom_pdf, chemin)

  message("✔ Fichiers exportes :")
  message("  ", nom_data)
  message("  ", nom_pdf)

  invisible(NULL)
}
