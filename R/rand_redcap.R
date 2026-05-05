
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

