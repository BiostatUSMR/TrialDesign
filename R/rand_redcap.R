###########################################################
# FONCTION INTERNE .rand_redcap()

.rand_redcap <- function(essai, seed) {

  k           <- essai$k
  block_sizes <- essai$block_sizes
  nb_block    <- essai$nb_block
  ratio       <- essai$ratio
  arm_label   <- essai$arm_label
  arm_code    <- essai$arm_code
  strat_vars  <- essai$strat_vars
  n           <- sum(block_sizes * nb_block)

  df <- data.frame()

  if (!is.null(strat_vars)) {
    grilles <- expand.grid(
      lapply(strat_vars, function(v) seq_along(v$codes)),
      stringsAsFactors = FALSE
    )

    for (r in seq_len(nrow(grilles))) {
      dfi <- .rando_bloc(n, k, seed + r, block_sizes, nb_block, ratio, arm_label, arm_code)

      for (s in seq_along(strat_vars)) {
        nom_var                        <- names(strat_vars)[s]
        idx                            <- grilles[r, s]
        dfi[[paste0("rdstr",     s)]] <- strat_vars[[nom_var]]$codes[idx]
        dfi[[paste0("rdstr_lib", s)]] <- strat_vars[[nom_var]]$labels[idx]
      }

      df <- rbind(df, dfi)
    }

  } else {
    df <- .rando_bloc(n, k, seed, block_sizes, nb_block, ratio, arm_label, arm_code)
  }

  df
}
