###########################################################
# FONCTION INTERNE .corresp_redcap()

.corresp_redcap <- function(essai, mini, maxi, seed, boi_label = NULL) {

  k         <- essai$k
  arm_code  <- essai$arm_code
  arm_label <- essai$arm_label

  set.seed(seed)
  boites     <- sample(mini:maxi)

  # Nombre de boites par bras
  n_par_bras <- rep(floor(length(boites) / k), k)
  # Gestion du reste éventuel
  reste <- length(boites) %% k
  if (reste > 0) {n_par_bras[seq_len(reste)] <- n_par_bras[seq_len(reste)] + 1}
  allocation <- rep(seq_len(k), times = n_par_bras)

  # Mélange aléatoire reproductible
  allocation <- sample(allocation)

  if (is.null(boi_label)) {
    rdboi_lib <- paste0("Boite de traitement n\u00b0", boites)
  } else {
    rdboi_lib <- paste0(boi_label, " n\u00b0", boites)
  }

  data.frame(
    rdboi     = boites,
    rdboi_lib = rdboi_lib,
    rdgrp     = arm_code[allocation],
    rdgrp_lib = arm_label[allocation],
    stringsAsFactors = FALSE
  )
}
