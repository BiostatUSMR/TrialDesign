
###########################################################
# FONCTION INTERNE .corresp_redcap()

.corresp_redcap <- function(mini, maxi, seed, boi_label = NULL) {

  k         <- .trialdesign_env$k
  arm_code  <- .trialdesign_env$arm_code
  arm_label <- .trialdesign_env$arm_label

  set.seed(seed)
  boites     <- sample(mini:maxi)
  allocation <- sample(seq_len(k), size = length(boites), replace = TRUE)

  if (is.null(boi_label)) {
    RDBOI_LIB <- paste0("Boîte de traitement n°", boites)
  } else {
    RDBOI_LIB <- paste0(boi_label, " n°", boites)
  }

  data.frame(
    RDBOI     = boites,
    RDBOI_LIB = RDBOI_LIB,
    RDGRP     = arm_code[allocation],
    RDGRP_LIB = arm_label[allocation],
    stringsAsFactors = FALSE
  )
}

