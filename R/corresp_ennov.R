###########################################################
# FONCTION INTERNE .corresp_ennov()

.corresp_ennov <- function(essai, mini, maxi, seed) {

  k                 <- essai$k
  arm_code          <- essai$arm_code
  arm_label         <- essai$arm_label
  conditionnements  <- essai$conditionnements

  set.seed(seed)

  boites <- sample(mini:maxi)

  # -------------------------------------------------------
  # Cas 1 : aucun conditionnement défini
  # -------------------------------------------------------

  if (is.null(conditionnements)) {

    # Nombre de boites par bras
    n_par_bras <- rep(floor(length(boites) / k), k)

    # Gestion du reste éventuel
    reste <- length(boites) %% k

    if (reste > 0) {
      n_par_bras[seq_len(reste)] <-
        n_par_bras[seq_len(reste)] + 1
    }

    allocation <- rep(seq_len(k), times = n_par_bras)

    # Mélange aléatoire reproductible
    allocation <- sample(allocation)

    return(
      data.frame(
        rdboi     = boites,
        rdgrp     = arm_code[allocation],
        rdgrp_lib = arm_label[allocation],
        stringsAsFactors = FALSE
      )
    )
  }

  # -------------------------------------------------------
  # Cas 2 : conditionnements définis
  # -------------------------------------------------------

  # Nombre total de boites demandé
  n_demande <- sum(
    sapply(conditionnements, function(cond) sum(cond$n))
  )

  # Nombre de boites disponibles
  n_disponible <- length(boites)

  if (n_demande != n_disponible) {
    stop(
      "Le nombre de boites disponibles (", n_disponible,
      ") ne correspond pas au nombre de boites demande (",
      n_demande, ") dans 'conditionnements'."
    )
  }

  # -------------------------------------------------------
  # Construction de la liste bras / conditionnement
  # -------------------------------------------------------

  allocation <- do.call(
    rbind,
    lapply(seq_len(k), function(i) {

      cond <- conditionnements[[i]]

      data.frame(
        bras = i,
        rdcond = rep(cond$code, cond$n),
        stringsAsFactors = FALSE
      )
    })
  )

  # -------------------------------------------------------
  # Mélange aléatoire global
  # -------------------------------------------------------

  allocation <- allocation[
    sample(seq_len(nrow(allocation))),
    ,
    drop = FALSE
  ]

  # -------------------------------------------------------
  # Attribution des boites
  # -------------------------------------------------------

  allocation$rdboi <- boites

  # -------------------------------------------------------
  # Informations sur le bras
  # -------------------------------------------------------

  allocation$rdgrp <- arm_code[allocation$bras]

  allocation$rdgrp_lib <- arm_label[allocation$bras]

  # -------------------------------------------------------
  # Libellé du conditionnement
  # -------------------------------------------------------

  allocation$rdcond_lib <- mapply(
    function(bras, code) {

      cond <- conditionnements[[bras]]

      cond$label[
        match(code, cond$code)
      ]

    },
    allocation$bras,
    allocation$rdcond,
    USE.NAMES = FALSE
  )

  # -------------------------------------------------------
  # Sélection et ordre des colonnes
  # -------------------------------------------------------

  allocation <- allocation[, c(
    "rdboi",
    "rdgrp",
    "rdgrp_lib",
    "rdcond",
    "rdcond_lib"
  )]

  rownames(allocation) <- NULL

  allocation
}
