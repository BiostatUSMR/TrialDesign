

# FONCTION INTERNE — .rando_bloc()

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



# FONCTION INTERNE — .rand_redcap() — Circuit REDCap

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

