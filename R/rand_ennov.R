###########################################################
# FONCTION INTERNE .rand_ennov()

.rand_ennov <- function(essai, seed) {

  k           <- essai$k
  block_sizes <- essai$block_sizes
  nb_block    <- essai$nb_block
  ratio       <- essai$ratio
  arm_label   <- essai$arm_label
  arm_code    <- essai$arm_code
  strat_vars  <- essai$strat_vars
  n           <- sum(block_sizes * nb_block)

  df <- data.frame()

  if (is.null(strat_vars)) {
    df <- .rando_bloc(n, k, seed, block_sizes, nb_block, ratio, arm_label, arm_code)

  } else {
    # Générer toutes les combinaisons de strates
    grilles <- expand.grid(
      lapply(strat_vars, function(v) seq_along(v$codes)),
      stringsAsFactors = FALSE
    )

    for (r in seq_len(nrow(grilles))) {

      # Code concaténé
      codes_r <- sapply(seq_along(strat_vars), function(s) {
        nom_var <- names(strat_vars)[s]
        idx     <- grilles[r, s]
        as.character(strat_vars[[nom_var]]$codes[idx])
      })
      strat_code_r <- as.integer(paste(codes_r, collapse = ""))

      # Libellé concaténé
      labels_r <- sapply(seq_along(strat_vars), function(s) {
        nom_var <- names(strat_vars)[s]
        idx     <- grilles[r, s]
        strat_vars[[nom_var]]$labels[idx]
      })
      strat_label_r <- paste(labels_r, collapse = " et ")

      dfi           <- .rando_bloc(n, k, seed + r, block_sizes, nb_block, ratio, arm_label, arm_code)
      dfi$RDSTR     <- strat_code_r
      dfi$RDSTR_LIB <- strat_label_r
      df            <- rbind(df, dfi)
    }
  }

  df
}
