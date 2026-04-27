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


sauvegarde <- function(df, format, nom_fichier = NULL) {

  # Vérification du format
  if (!format %in% c("csv", "txt")) {
    stop("Les formats autorisés sont 'csv' et 'txt'.")}

  # Nom du fichier par défaut
  if (is.null(nom_fichier)) {
    nom_fichier <- paste0("liste_randomisation_", Sys.Date(),".",format)}

  # Export
  if (format == "txt") {
    write.table(df, file = nom_fichier, row.names = FALSE, sep = "\t")
  }

  if (format == "csv") {
    write.csv(df, file = nom_fichier, row.names = FALSE)
  }

  message("Liste exportée : ", nom_fichier)
}

