correspo <- function(i, j, k, seed,
                                 arm_label = NULL,
                                 arm_code  = NULL) {

  # Valeurs par défaut
  if (is.null(arm_label)) { arm_label <- paste0("Groupe", 1:k) }
  if (is.null(arm_code))  { arm_code  <- 1:k }

  # Vérifications
  if (!is.numeric(i) || i != round(i) || i <= 0) {
    stop("'i' doit être un entier strictement positif.")
  }
  if (!is.numeric(j) || j != round(j) || j <= 0) {
    stop("'j' doit être un entier strictement positif.")
  }
  if (j <= i) {
    stop("'j' doit être strictement supérieur à 'i'.")
  }
  if (!is.numeric(k) || k < 2 || k != round(k)) {
    stop("'k' doit être un entier >= 2.")
  }
  if (!is.numeric(seed) || seed != round(seed)) {
    stop("'seed' doit être un entier.")
  }
  if (!is.null(arm_label) && length(arm_label) != k) {
    stop("La longueur de 'arm_label' doit être égale à k.")
  }
  if (!is.null(arm_code)) {
    if (!is.numeric(arm_code) || length(arm_code) != k) {
      stop("'arm_code' doit être un vecteur numérique de longueur k.")
    }
  }

  # Fixer la graine
  set.seed(seed)

  # Mélanger les numéros de boîtes
  boites <- sample(i:j)

  # Attribuer aléatoirement un traitement
  allocation <- sample(1:k, size = length(boites), replace = TRUE)

  # Construire le data.frame
  df <- data.frame(
    RDBOI     = boites,
    RDGRP     = arm_code[allocation],
    RDGRP_LIB = arm_label[allocation],
    stringsAsFactors = FALSE
  )

  return(df)
}

