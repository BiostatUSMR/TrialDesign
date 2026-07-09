#' FONCTION rand()
#'
#' @description
#' Genere la liste de randomisation selon le circuit defini dans l'objet
#' \code{essai} cree par \code{init_essai()}, exporte automatiquement deux
#' fichiers (donnees + PDF), et retourne le data.frame genere pour
#' inspection visuelle.
#'
#' @param essai Liste. Objet cree par \code{init_essai()}.
#'   Exemple : \code{essai <- init_essai(...)}.
#' @param seed Entier. Graine aleatoire pour la reproductibilite.
#' @param statut Caractere. Statut de la liste generee : \code{"FINALE"} ou
#'   \code{"FICTIVE"}.
#' @param version Caractere. Version de la liste generee. Ex : \code{"v01"}.
#' @param col_widths Vecteur de caracteres. Largeurs des colonnes du tableau PDF.
#'   Ex : \code{c("2cm", "3cm", "4cm")}. Si NULL, largeurs automatiques.
#'   Doit avoir autant d'elements que de colonnes dans le data.frame de sortie.
#' @param chemin Caractere. Chemin vers le repertoire de sortie.
#'   Si NULL, repertoire de travail courant.
#'
#' @importFrom utils write.csv write.table
#' @importFrom rmarkdown render
#'
#' @return Le data.frame de la liste de randomisation (retoure visiblement
#'   pour inspection). Les fichiers sont exportes en parallele.
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
#' df <- rand(essai, seed = 42, statut = "FICTIVE", version = "v01")
#' head(df)
#'
#' # Deux essais en parallele sans conflit
#' essai_A <- init_essai("ESSAI_A", circuit = "ennov", ...)
#' essai_B <- init_essai("ESSAI_B", circuit = "redcap", ...)
#' df_A <- rand(essai_A, seed = 42, statut = "FICTIVE", version = "v01")
#' df_B <- rand(essai_B, seed = 99, statut = "FICTIVE", version = "v01")
#' }
#'
#' @export

rand <- function(essai, seed, statut, version,
                 col_widths = NULL,
                 chemin     = NULL) {

  # --- Verification de l'objet essai ---
  if (!is.list(essai) || is.null(essai$circuit)) {
    stop(
      "'essai' doit etre un objet cree par init_essai().\n",
      "Exemple : essai <- init_essai(...) puis rand(essai, seed = 42, ...)"
    )
  }

  circuit   <- essai$circuit
  nom_etude <- essai$nom_etude

  # --- Valeur par defaut ---
  if (is.null(chemin)) chemin <- getwd()

  # --- Verifications ---
  if (!is.numeric(seed) || seed != round(seed))
    stop("'seed' doit etre un entier.")

  if (!statut %in% c("FINALE", "FICTIVE"))
    stop("'statut' doit etre 'FINALE' ou 'FICTIVE'.")

  if (!dir.exists(chemin))
    stop("Le chemin specifie n'existe pas.")

  if (!is.null(col_widths) && !is.character(col_widths))
    stop("'col_widths' doit etre un vecteur de caracteres Ex : c('2cm', '3cm').")

  # --- Generation du data.frame ---
  df <- if (circuit == "ennov") .rand_ennov(essai, seed) else .rand_redcap(essai, seed)

  message("\u2714 Liste de randomisation generee ", nrow(df), " sujets.")

  # --- Nom de base des fichiers ---
  ext      <- if (circuit == "redcap") "csv" else "txt"
  date_str <- format(Sys.Date(), "%Y%m%d")
  nom_base <- paste0(nom_etude, " - Liste de randomisation ",
                     statut, " - ", version, " - ", date_str)

  # --- Export donnees ---
  nom_data <- file.path(chemin, paste0(nom_base, ".", ext))

  if (circuit == "redcap") {
    cols_rdstr <- grep("^rdstr[0-9]*$", names(df), value = TRUE)
    df_csv     <- df[, c("rdnum", "rdgrp", cols_rdstr), drop = FALSE]
    names(df_csv) <- c("redcap_randomization_number", "rdgrp", tolower(cols_rdstr))
    write.csv(df_csv, file = nom_data, row.names = FALSE, quote = FALSE)
  } else {
    cols_rdstr <- grep("^rdstr\\d*$", names(df), value = TRUE)
    df_txt     <- df[, c("rdnum", "rdgrp", cols_rdstr), drop = FALSE]
    write.table(df_txt, file = nom_data, row.names = FALSE,
                col.names = FALSE, sep = ";")
  }

  # --- Export PDF ---
  nom_pdf <- paste0(nom_base, ".pdf")
  .export_pdf(essai, df, nom_pdf, chemin,
              type_doc   = "randomisation",
              version_doc = version,
              col_widths = col_widths)

  message("\u2714 Fichiers exportes :")
  message("  ", nom_data)
  message("  ", file.path(chemin, nom_pdf))

  # --- Stockage du dataframe dans l'environnement ---
  assign("df_randomisation", df, envir = parent.frame())
  invisible(df)
}
