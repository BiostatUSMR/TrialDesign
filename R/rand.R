#' Generate a randomization list
#'
#' @description
#' Generates a randomization list according to the randomization system defined in the
#' \code{essai} object created with \code{\link{init_essai}}. The
#' generated list is automatically exported as a data file and a PDF document
#' and is also returned as a data frame for inspection.
#'
#' @param essai List. Trial object created with \code{\link{init_essai}}.
#'   For example: \code{essai <- init_essai(...)}.
#' @param seed Integer. Random seed used to ensure reproducibility.
#' @param statut Character string. Status of the generated list: \code{"FINALE"} or \code{"FICTIVE"}
#' @param version Character string. Version of the generated list. For example, \code{"v01"}.
#' @param col_widths Character vector. Widths of the columns in the generated PDF table.
#'   For example, \code{c("2cm", "3cm", "4cm")}. If \code{NULL}, column widths are
#'   automatically determined. The vector must contain one element for each column of the generated data frame.
#' @param chemin Character string. Path to the output directory. If \code{NULL}, the current working directory is used.
#'
#' @importFrom utils write.csv write.table
#' @importFrom rmarkdown render
#'
#' @return A data frame containing the generated randomization list. The list is also
#' exported as a data file and a PDF document.
#'
#' @examples
#' \dontrun{
#' essai <- init_essai(
#'   nom_etude = "ESSAI_ABC", circuit = "ennov", k = 2,
#'   block_sizes = c(4), nb_block = c(10),
#'   arm_label = c("Placebo", "Traitement"), arm_code = c(0, 1)
#' )
#'
#' # Inspect the generated data frame and export the files
#' df <- rand(essai, seed = 42, statut = "FICTIVE", version = "v01")
#' head(df)
#'
#' # Run two independent trials in parallel without conflicts
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
