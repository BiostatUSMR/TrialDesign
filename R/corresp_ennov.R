
###########################################################
# FONCTION INTERNE .corresp_ennov()

.corresp_ennov <- function(mini, maxi, seed) {

  k         <- .trialdesign_env$k
  arm_code  <- .trialdesign_env$arm_code
  arm_label <- .trialdesign_env$arm_label

  set.seed(seed)
  boites     <- sample(mini:maxi)
  allocation <- sample(seq_len(k), size = length(boites), replace = TRUE)

  data.frame(
    RDBOI     = boites,
    RDGRP     = arm_code[allocation],
    RDGRP_LIB = arm_label[allocation],
    stringsAsFactors = FALSE
  )
}

