#' FONCTION rand()
#'
#' @description
#' Genere la liste de randomisation selon le circuit defini dans
#' init_essai(), puis exporte automatiquement deux fichiers :
#' un fichier de donnees (.txt pour Ennov, .csv pour REDCap)
#' un fichier PDF avec les libelles lisibles et une page de garde.
#'
#' @param seed Entier. Graine aleatoire pour la reproductibilite.
#' @param statut Caractere. Statut de la liste generee "FINALE" ou "FICTIVE".
#' @param version Caractere. Version de la liste generee. Ex : "V01".
#' @param col_widths Vecteur de caracteres. Largeurs des colonnes du tableau PDF.
#'   Ex : c("2cm", "3cm", "4cm"). Si NULL, largeurs automatiques.
#'   Doit avoir autant d'elements que de colonnes dans le data.frame de sortie.
#' @param chemin Caractere. Chemin vers le repertoire de sortie.
#'   Si NULL, repertoire de travail courant.
#'
#' @importFrom utils write.csv write.table
#' @importFrom rmarkdown render
#'
#' @return Invisible. Exporte les fichiers et affiche leur chemin.
#'
#' @examples
#' \dontrun{
#' # Sans largeurs personnalisees
#' rand(seed = 42, statut = "FICTIVE", version = "V01")
#'
#' # Avec largeurs personnalisees
#' rand(seed = 42, statut = "FICTIVE", version = "V01",
#'      col_widths = c("1.5cm", "2cm", "4cm", "2cm", "4cm"))
#' }
#'
#' @export

rand <- function(seed, statut, version,
                 col_widths = NULL,
                 chemin     = NULL) {

  circuit    <- .trialdesign_env$circuit
  nom_etude  <- .trialdesign_env$nom_etude

  # Valeur par defaut
  if (is.null(chemin)) chemin <- getwd()

  # Verifications
  if (!is.numeric(seed) || seed != round(seed)) {
    stop("'seed' doit etre un entier.")
  }
  if (!statut %in% c("FINALE", "FICTIVE")) {
    stop("'statut' doit etre 'FINALE' ou 'FICTIVE'.")
  }
  if (!dir.exists(chemin)) {
    stop("Le chemin specifie n'existe pas.")
  }
  if (!is.null(col_widths) && !is.character(col_widths)) {
    stop("'col_widths' doit etre un vecteur de caracteres. Ex : c('2cm', '3cm').")
  }

  # Generation du data.frame
  df <- if (circuit == "ennov") .rand_ennov(seed) else .rand_redcap(seed)

  message("\u2714 Liste de randomisation generee - ", nrow(df), " sujets.")

  # Nom de base des fichiers
  ext      <- if (circuit == "redcap") "csv" else "txt"
  date_str <- format(Sys.Date(), "%Y%m%d")
  nom_base <- paste0(nom_etude, " - Liste de randomisation ",
                     statut, " - ", version, " - ", date_str)

  # Export donnees
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
  nom_pdf <- paste0(nom_base, ".pdf")
  .export_pdf(df, nom_pdf, chemin,
              type_doc   = "randomisation",
              col_widths = col_widths)

  message("\u2714 Fichiers exportes :")
  message("  ", nom_data)
  message("  ", nom_pdf)

  invisible(NULL)
}
