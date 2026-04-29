
###########################################################
# FONCTION INTERNE .rando_bloc()

.rando_bloc <- function(n, k, seed, block_sizes, nb_block, ratio, arm_label, arm_code) {

  # Fixer la graine
  set.seed(seed)

  # Génération de l'allocation
  allocation = c()

  # Liste aléatoire des m blocs avec leurs taille
  m <- sum(nb_block)
  block_list=c()
  for (i in 1:length(block_sizes)){
    x= rep(block_sizes[i], times=nb_block[i])
    block_list <- c(block_list,x)
  }
  block_list <- sample(block_list)

  # Génération du contenu de chaque bloc
  for (j in 1:m){
    contenu <- rep(1:k, times = ratio/sum(ratio)*block_list[j])
    bloc <- sample(contenu)

    # Sequence finale
    allocation <- c(allocation, bloc)
  }

  # Construire le data.frame
  df <- data.frame(
    RDNUM= 1:n,
    RDGRP = arm_code[allocation],
    RDGRP_LIB  = arm_label[allocation],
    stringsAsFactors = FALSE
  )


}


###########################################################
# FONCTION INTERNE .rand_ennov()

.rand_ennov <- function(seed) {

  k           <- .trialdesign_env$k
  block_sizes <- .trialdesign_env$block_sizes
  nb_block    <- .trialdesign_env$nb_block
  ratio       <- .trialdesign_env$ratio
  arm_label   <- .trialdesign_env$arm_label
  arm_code    <- .trialdesign_env$arm_code
  strat_vars  <- .trialdesign_env$strat_vars
  n           <- sum(block_sizes * nb_block)

  df <- data.frame()

  if (is.null(strat_vars)) {
    df           <- .rando_bloc(n, k, seed, block_sizes, nb_block, ratio, arm_label, arm_code) }

  else {
    # Générer toutes les combinaisons de strates
    grilles <- expand.grid(
      lapply(strat_vars, function(v) seq_along(v$codes)),
      stringsAsFactors = FALSE
    )

    for (r in seq_len(nrow(grilles))) {

      # Code concaténé : collage textuel des codes de chaque variable
      codes_r <- sapply(seq_along(strat_vars), function(s) {
        nom_var <- names(strat_vars)[s]
        idx     <- grilles[r, s]
        as.character(strat_vars[[nom_var]]$codes[idx])
      })
      strat_code_r <- as.integer(paste(codes_r, collapse = ""))

      # Label concaténé : séparateur " et "
      labels_r <- sapply(seq_along(strat_vars), function(s) {
        nom_var <- names(strat_vars)[s]
        idx     <- grilles[r, s]
        strat_vars[[nom_var]]$labels[idx]
      })
      strat_label_r <- paste(labels_r, collapse = " et ")

      dfi           <- .rando_bloc(n, k, seed+r, block_sizes, nb_block, ratio, arm_label, arm_code)
      dfi$RDSTR     <- strat_code_r
      dfi$RDSTR_LIB <- strat_label_r
      df            <- rbind(df, dfi)
    }
  }

  df
}


###########################################################
# FONCTION INTERNE  .rand_redcap()

.rand_redcap <- function(seed) {

  k           <- .trialdesign_env$k
  block_sizes <- .trialdesign_env$block_sizes
  nb_block    <- .trialdesign_env$nb_block
  ratio       <- .trialdesign_env$ratio
  arm_label   <- .trialdesign_env$arm_label
  arm_code    <- .trialdesign_env$arm_code
  strat_vars  <- .trialdesign_env$strat_vars
  n           <- sum(block_sizes * nb_block)

  df <- data.frame()

  if (!is.null(strat_vars)) {
    grilles <- expand.grid(
      lapply(strat_vars, function(v) seq_along(v$codes)),
      stringsAsFactors = FALSE
    )

    for (r in seq_len(nrow(grilles))) {
      dfi <- .rando_bloc(n, k, seed + r , block_sizes, nb_block,
                         ratio, arm_label, arm_code)

      for (s in seq_along(strat_vars)) {
        nom_var                        <- names(strat_vars)[s]
        idx                            <- grilles[r, s]
        dfi[[paste0("RDSTR",     s)]] <- strat_vars[[nom_var]]$codes[idx]
        dfi[[paste0("RDSTR_LIB", s)]] <- strat_vars[[nom_var]]$labels[idx]
      }

      df <- rbind(df, dfi)
    }

  } else {
    df <- .rando_bloc(n, k, seed, block_sizes, nb_block, ratio, arm_label, arm_code) }

  df
}



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
#' rand(seed = 42, type = "FICTIVE", version = "V1")
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
    df_txt     <- df[, c("RDNUM", "RDGRP", cols_rdstr), drop = FALSE]
    write.table(df_txt, file = nom_data, row.names = FALSE,
                col.names = FALSE, sep = "\t")
  }

  # Export PDF
  nom_pdf   <- file.path(chemin, paste0(nom_base, ".pdf"))

  .export_pdf(df, nom_pdf, chemin)

  message("✔ Fichiers exportes :")
  message("  ", nom_data)
  message("  ", nom_pdf)

  invisible(NULL)
}
