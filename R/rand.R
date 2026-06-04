#' FONCTION rand()
#'
#' @description
#' Génère la liste de randomisation selon le circuit défini dans l'objet
#' \code{essai} créé par \code{init_essai()}, exporte automatiquement deux
#' fichiers (données + PDF), et retourne le data.frame généré pour
#' inspection visuelle.
#'
#' @param essai Liste. Objet créé par \code{init_essai()}.
#'   Exemple : \code{essai <- init_essai(...)}.
#' @param seed Entier. Graine aléatoire pour la reproductibilité.
#' @param statut Caractère. Statut de la liste générée : \code{"FINALE"} ou
#'   \code{"FICTIVE"}.
#' @param version Caractère. Version de la liste générée. Ex : \code{"v01"}.
#' @param col_widths Vecteur de caractères. Largeurs des colonnes du tableau PDF.
#'   Ex : \code{c("2cm", "3cm", "4cm")}. Si NULL, largeurs automatiques.
#'   Doit avoir autant d'éléments que de colonnes dans le data.frame de sortie.
#' @param chemin Caractère. Chemin vers le répertoire de sortie.
#'   Si NULL, répertoire de travail courant.
#'
#' @importFrom utils write.csv write.table
#' @importFrom rmarkdown render
#'
#' @return Le data.frame de la liste de randomisation (retouré visiblement
#'   pour inspection). Les fichiers sont exportés en parallèle.
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
#' # Deux essais en parallèle sans conflit
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

  # --- Vérification de l'objet essai ---
  if (!is.list(essai) || is.null(essai$circuit)) {
    stop(
      "'essai' doit \u00eatre un objet cr\u00e9\u00e9 par init_essai().\n",
      "Exemple : essai <- init_essai(...) puis rand(essai, seed = 42, ...)"
    )
  }

  circuit   <- essai$circuit
  nom_etude <- essai$nom_etude

  # --- Valeur par défaut ---
  if (is.null(chemin)) chemin <- getwd()

  # --- Vérifications ---
  if (!is.numeric(seed) || seed != round(seed))
    stop("'seed' doit \u00eatre un entier.")

  if (!statut %in% c("FINALE", "FICTIVE"))
    stop("'statut' doit \u00eatre 'FINALE' ou 'FICTIVE'.")

  if (!dir.exists(chemin))
    stop("Le chemin sp\u00e9cifi\u00e9 n'existe pas.")

  if (!is.null(col_widths) && !is.character(col_widths))
    stop("'col_widths' doit \u00eatre un vecteur de caract\u00e8res. Ex : c('2cm', '3cm').")

  # --- Génération du data.frame ---
  df <- if (circuit == "ennov") .rand_ennov(essai, seed) else .rand_redcap(essai, seed)

  message("\u2714 Liste de randomisation g\u00e9n\u00e9r\u00e9e \u2014 ", nrow(df), " sujets.")

  # --- Nom de base des fichiers ---
  ext      <- if (circuit == "redcap") "csv" else "txt"
  date_str <- format(Sys.Date(), "%Y%m%d")
  nom_base <- paste0(nom_etude, " - Liste de randomisation ",
                     statut, " - ", version, " - ", date_str)

  # --- Export données ---
  nom_data <- file.path(chemin, paste0(nom_base, ".", ext))

  if (circuit == "redcap") {
    write.csv(df, file = nom_data, row.names = FALSE)
  } else {
    cols_rdstr <- grep("^RDSTR\\d*$", names(df), value = TRUE)
    df_txt     <- df[, c(cols_rdstr, "RDNUM", "RDGRP"), drop = FALSE]
    write.table(df_txt, file = nom_data, row.names = FALSE,
                col.names = FALSE, sep = ";")
  }

  # --- Export PDF ---
  nom_pdf <- paste0(nom_base, ".pdf")
  .export_pdf(essai, df, nom_pdf, chemin,
              type_doc   = "randomisation",
              col_widths = col_widths)

  message("\u2714 Fichiers export\u00e9s :")
  message("  ", nom_data)
  message("  ", file.path(chemin, nom_pdf))

  # --- Retour visible du data.frame pour inspection ---
  return(df)
}
