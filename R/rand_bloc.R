###########################################################
# FONCTION INTERNE .rando_bloc()

.rando_bloc <- function(n, k, seed, block_sizes, nb_block, ratio, arm_label, arm_code) {

  set.seed(seed)

  allocation <- c()

  # Liste aléatoire des blocs avec leurs tailles
  m          <- sum(nb_block)
  block_list <- c()
  for (i in seq_along(block_sizes)) {
    block_list <- c(block_list, rep(block_sizes[i], times = nb_block[i]))
  }
  block_list <- sample(block_list)

  # Génération du contenu de chaque bloc
  for (j in seq_len(m)) {
    contenu    <- rep(1:k, times = ratio / sum(ratio) * block_list[j])
    bloc       <- sample(contenu)
    allocation <- c(allocation, bloc)
  }

  data.frame(
    RDNUM     = 1:n,
    RDGRP     = arm_code[allocation],
    RDGRP_LIB = arm_label[allocation],
    stringsAsFactors = FALSE
  )
}
