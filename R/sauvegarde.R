#' Sauvegarder une liste de randomisation
#'
#' @description
#' Exporte un data.frame issu d'une fonction de randomisation dans un fichier
#' au format choisi par l'utilisateur. Le nom du fichier peut être personnalisé ;
#' l'extension est ajoutée automatiquement selon le format sélectionné.
#'
#' @param df Data.frame à exporter
#' @param format Format d'export souhaité. Valeurs acceptées :csv et txt
#' @param nom_fichier Nom du fichier sans extension.
#'  Si NULL, un nom est généré
#'   automatiquement sous la forme "liste_randomisation_AAAA-MM-JJ".
#'
#' @returns Aucune valeur retournée
#'
#' @export
#'
#' @examples
#' # Générer une liste de randomisation
#' ma_liste <- rando_bloc(n = 40, k = 2, seed = 42,
#'                        block_sizes = c(4), nb_block = c(10))
#' # Export TXT avec nom personnalisé
#' sauvegarder(ma_liste, format = "txt", nom_fichier = "essai_pilote")


sauvegarde <- function(df, format, nom_etude, type, version) {

  # Vérification du format
  if (!format %in% c("csv", "txt", "pdf")) {
    stop("Les formats autorisés sont 'csv', 'txt' et 'pdf'.")
  }

  # Nom de base du fichier
  nom_base <- paste0(nom_etude, " - Liste de randomisation ",
                     type, " - ", version, " - ",
                     format(Sys.Date(), "%Y%m%d"))

  nom_fichier <- paste0(nom_base, ".", format)

  # Export CSV
  if (format == "csv") {
    write.csv(df, file = nom_fichier, row.names = FALSE)
  }

  # Export TXT
  if (format == "txt") {
    write.table(df, file = nom_fichier, row.names = FALSE,
                sep = "\t", col.names = FALSE)
  }

  # Export PDF
  if (format == "pdf") {

    # Renommer les colonnes avec les libellés pour le PDF
    df_pdf <- df
    colnames(df_pdf) <- c(
      "Numéro de randomisation",
      "Code Traitement",
      "Libellé Traitement",
      "Code Strate",
      "Libellé Strate"
    )[1:ncol(df_pdf)]

    # Créer le fichier Rmd temporaire
    rmd_temp <- tempfile(fileext = ".Rmd")

    writeLines(c(
      "---",
      paste0('title: "', nom_etude, '"'),
      paste0('subtitle: "Liste de randomisation ', type,
             ' - ', version, ' - ', format(Sys.Date(), "%Y%m%d"), '"'),
      "output:",
      "  pdf_document:",
      "    latex_engine: xelatex",
      "geometry: margin=2cm",
      "---",
      "",
      "```{r echo=FALSE, message=FALSE, warning=FALSE}",
      "library(knitr)",
      "kable(df_pdf, align = 'c', row.names = FALSE)",
      "```"
    ), rmd_temp)

    # Compiler le PDF
    rmarkdown::render(
      input       = rmd_temp,
      output_file = paste0(nom_base, ".pdf"),
      output_dir  = getwd(),
      envir       = environment(),
      quiet       = TRUE
    )

    # Supprimer le fichier temporaire
    file.remove(rmd_temp)
  }

  message("Liste exportée : ", nom_fichier)
}



ma_liste <- rando(k = 2, seed = 42, block_sizes = c(4),
                  nb_block = c(10))

# Export PDF
sauvegarde(ma_liste, format = "pdf",
           nom_etude = "ESSAI_PILOTE",
           type = "FICTIVE",
           version = "v01")

