###########################################################
# FONCTION INTERNE .corresp_ennov()

.corresp_ennov <- function(essai, mini, maxi, seed) {

  k         <- essai$k
  arm_code  <- essai$arm_code
  arm_label <- essai$arm_label

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
