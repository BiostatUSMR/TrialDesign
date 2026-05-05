
###########################################################
# FONCTION INTERNE .rando_bloc()

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

