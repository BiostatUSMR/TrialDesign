
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

