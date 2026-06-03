###########################################################
# FONCTION INTERNE .corresp_redcap()

.corresp_redcap <- function(essai, mini, maxi, seed, boi_label = NULL) {

  k         <- essai$k
  arm_code  <- essai$arm_code
  arm_label <- essai$arm_label

  set.seed(seed)
  boites     <- sample(mini:maxi)
  allocation <- sample(seq_len(k), size = length(boites), replace = TRUE)

  if (is.null(boi_label)) {
    RDBOI_LIB <- paste0("Bo\u00eete de traitement n\u00b0", boites)
  } else {
    RDBOI_LIB <- paste0(boi_label, " n\u00b0", boites)
  }

  data.frame(
    RDBOI     = boites,
    RDBOI_LIB = RDBOI_LIB,
    RDGRP     = arm_code[allocation],
    RDGRP_LIB = arm_label[allocation],
    stringsAsFactors = FALSE
  )
}
