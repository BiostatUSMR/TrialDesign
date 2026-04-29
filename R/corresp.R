
###########################################################
# FONCTION INTERNE .corresp_ennov()

.corresp_ennov <- function(i, j, seed) {

  k         <- .trialdesign_env$k
  arm_code  <- .trialdesign_env$arm_code
  arm_label <- .trialdesign_env$arm_label

  set.seed(seed)
  boites     <- sample(i:j)
  allocation <- sample(seq_len(k), size = length(boites), replace = TRUE)

  data.frame(
    RDBOI     = boites,
    RDGRP     = arm_code[allocation],
    RDGRP_LIB = arm_label[allocation],
    stringsAsFactors = FALSE
  )
}


###########################################################
# FONCTION INTERNE .corresp_redcap()

.corresp_redcap <- function(i, j, seed, boi_label = NULL) {

  k         <- .trialdesign_env$k
  arm_code  <- .trialdesign_env$arm_code
  arm_label <- .trialdesign_env$arm_label

  set.seed(seed)
  boites     <- sample(i:j)
  allocation <- sample(seq_len(k), size = length(boites), replace = TRUE)

  if (is.null(boi_label)) {
    RDBOI_LIB <- paste0("Boîte de traitement n°", boites)
  } else {
    RDBOI_LIB <- paste0(boi_label, " n°", boites)
  }

  data.frame(
    RDBOI     = boites,
    RDBOI_LIB = RDBOI_LIB,
    RDGRP     = arm_code[allocation],
    RDGRP_LIB = arm_label[allocation],
    stringsAsFactors = FALSE
  )
}


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
#' @param i Entier. Premier numéro de boîte.
#' @param j Entier. Dernier numéro de boîte.
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
#' corresp(i = 1, j = 50, seed = 42, type = "FICTIVE", version = "v01",
#'         boi_label = "Kit")
#'
#'}
#' @export

corresp <- function(i, j, seed, statut, version,
                    boi_label = NULL,
                    chemin    = NULL) {

  circuit   <- .trialdesign_env$circuit
  nom_etude <- .trialdesign_env$nom_etude

  # Valeurs par défaut
  if (is.null(chemin)) chemin <- getwd()

  # Vérifications
  if (!is.numeric(i) || i != round(i) || i <= 0) {
    stop("'i' doit être un entier strictement positif.")
  }
  if (!is.numeric(j) || j != round(j) || j <= 0) {
    stop("'j' doit être un entier strictement positif.")
  }
  if (j <= i) {
    stop("'j' doit être strictement supérieur à 'i'.")
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
    .corresp_ennov(i, j, seed)
  } else {
    .corresp_redcap(i, j, seed, boi_label)
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
  nom_pdf <- file.path(chemin, paste0(nom_base, ".pdf"))
  .export_pdf(df, nom_pdf, chemin)

  message("✔ Fichiers exportes :")
  message("  ", nom_data)
  message("  ", nom_pdf)

  invisible(NULL)
}


