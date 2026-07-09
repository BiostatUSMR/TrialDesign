#' FONCTION corresp()
#'
#' @description
#' Genere la liste de correspondance boites/traitements selon le circuit
#' defini dans l'objet \code{essai} cree par \code{init_essai()}, exporte
#' automatiquement trois fichiers (donnees + PDF + XLS pour le circuit ENNOV),
#' et retourne le data.frame genere pour inspection visuelle.
#'
#' @param essai Liste. Objet cree par \code{init_essai()}.
#'   Exemple : \code{essai <- init_essai(...)}.
#' @param mini Entier. Premier numero de boite.
#' @param maxi Entier. Dernier numero de boite.
#' @param seed Entier. Graine aleatoire pour la reproductibilite.
#' @param statut Caractere. Statut de la liste generee : \code{"FINALE"} ou
#'   \code{"FICTIVE"}.
#' @param version Caractere. Version de la liste generee. Ex : \code{"v01"}.
#' @param boi_label Caractere. Libelle personnalise pour les boites.
#'   Si NULL, \code{"Boite de traitement numero xxx"} est utilise.
#'   Utilise uniquement en circuit REDCap.
#' @param col_widths Vecteur de caracteres. Largeurs des colonnes du tableau PDF.
#'   Ex : \code{c("2cm", "3cm", "4cm")}. Si NULL, largeurs automatiques.
#' @param chemin Caractere. Chemin vers le repertoire de sortie.
#'   Si NULL, repertoire de travail courant.
#'
#' @return Le data.frame de la liste de correspondance (retourne visiblement
#'   pour inspection). Les fichiers suivants sont exportes en parallele :
#'   \itemize{
#'     \item Fichier de donnees (.txt pour Ennov, .csv pour REDCap)
#'     \item Fichier PDF avec page de garde institutionnelle
#'     \item Fichier XLS (pour ENNOV uniquement)
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

  # --- Verification de l'objet essai ---
  if (!is.list(essai) || is.null(essai$circuit)) {
    stop(
      "'essai' doit etre un objet cree par init_essai().\n",
      "Exemple : essai <- init_essai(...) puis corresp(essai, mini = 1, maxi = 50, ...)"
    )
  }

  circuit   <- essai$circuit
  nom_etude <- essai$nom_etude
  arm_label <- essai$arm_label
  arm_code  <- essai$arm_code

  if (is.null(chemin)) chemin <- getwd()

  # --- Verifications ---
  if (!is.numeric(mini) || mini != round(mini) || mini <= 0)
    stop("'mini' doit etre un entier strictement positif.")

  if (!is.numeric(maxi) || maxi != round(maxi) || maxi <= 0)
    stop("'maxi' doit etre un entier strictement positif.")

  if (maxi <= mini)
    stop("'maxi' doit etre strictement superieur a 'mini'.")

  if (!is.numeric(seed) || seed != round(seed))
    stop("'seed' doit etre un entier.")

  if (!statut %in% c("FINALE", "FICTIVE"))
    stop("'statut' doit etre 'FINALE' ou 'FICTIVE'.")

  if (!dir.exists(chemin))
    stop("Le chemin specifie n'existe pas.")

  if (!is.null(boi_label) && !is.character(boi_label))
    stop("'boi_label' doit etre une chaine de caracteres.")

  if (!is.null(col_widths) && !is.character(col_widths))
    stop("'col_widths' doit etre un vecteur de caracteres. Ex : c('2cm', '3cm').")

  # --- Generation selon le circuit ---
  df <- if (circuit == "ennov") {
    .corresp_ennov(essai, mini, maxi, seed)
  } else {
    .corresp_redcap(essai, mini, maxi, seed, boi_label)
  }

  message("\u2714 Liste de correspondance generee \u2014 ", nrow(df), " boites.")

  # --- Nom de base des fichiers ---
  ext      <- if (circuit == "redcap") "csv" else "txt"
  date_str <- format(Sys.Date(), "%Y%m%d")
  nom_base <- paste0(nom_etude, " - Liste de correspondance ",
                     statut, " - ", version, " - ", date_str)

  # --- Export donnees (TXT ou CSV) ---
  nom_data <- file.path(chemin, paste0(nom_base, ".", ext))

  if (circuit == "redcap") {
    df_csv <- df[, c("rdboi", "rdboi_lib")]
    names(df_csv) <- c("rdboi", "rdboi_lib")

    # BOM UTF-8 ecrit en mode binaire (writeChar sur connexion texte n'est
    # pas fiable pour ecrire une sequence d'octets precise comme le BOM)
    con_bin <- file(nom_data, open = "wb")
    writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con_bin)
    close(con_bin)

    # Contenu CSV ajoute ensuite, via une connexion texte en UTF-8
    con_txt <- file(nom_data, open = "a", encoding = "UTF-8")
    write.csv(df_csv, file = con_txt, row.names = FALSE, quote = FALSE)
    close(con_txt)
  } else {
    write.table(df[, c("rdboi", "rdgrp")],
                file      = nom_data,
                row.names = FALSE,
                col.names = FALSE,
                sep       = ";")
  }

  # --- Export PDF ---
  nom_pdf <- paste0(nom_base, ".pdf")
  .export_pdf(essai, df, nom_pdf, chemin,
              type_doc   = "correspondance",
              version_doc = version,
              col_widths = col_widths)

  # --- Export XLS (ENNOV uniquement) ---
  nom_xls <- NULL
  if (circuit == "ennov") {
    nom_xls <- file.path(chemin, paste0(nom_base, ".xlsx"))
    .export_corresp_xls(df, nom_xls, arm_label, arm_code)
  }

  message("\u2714 Fichiers exportes :")
  message("  ", nom_data)
  message("  ", file.path(chemin, nom_pdf))
  if (!is.null(nom_xls)) message("  ", nom_xls)

  # --- Stockage du dataframe dans l'environnement ---
  assign("df_correspondance", df, envir = parent.frame())
  invisible(df)
}
