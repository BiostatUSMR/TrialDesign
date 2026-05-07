init_essai <- function(nom_etude,
                       circuit,
                       k,
                       block_sizes,
                       nb_block,
                       ratio             = NULL,
                       arm_label         = NULL,
                       arm_code          = NULL,
                       strat_vars        = NULL,
                       id_etude          = NULL,
                       libelle_etude     = NULL,
                       investigateur     = NULL,
                       methodologiste    = NULL,
                       biostatisticien   = NULL,
                       confidentiel      = FALSE,
                       code_usmr         = NULL,
                       indice_document   = NULL) {

  # --- Vérification circuit ---
  if (!circuit %in% c("ennov", "redcap")) {
    stop("'circuit' doit être 'ennov' ou 'redcap'.")
  }

  # --- Valeurs par défaut ---
  if (is.null(ratio))     ratio     <- rep(1, k)
  if (is.null(arm_label)) arm_label <- paste0("Groupe", 1:k)
  if (is.null(arm_code))  arm_code  <- 1:k

  # --- Vérifications arguments randomisation ---
  if (!is.numeric(k) || k < 2 || k != round(k))
    stop("'k' doit être un entier >= 2.")

  if (!is.numeric(block_sizes) || any(block_sizes <= 0))
    stop("'block_sizes' doit contenir des entiers strictement positifs.")

  if (!is.numeric(nb_block) || any(nb_block <= 0))
    stop("'nb_block' doit contenir des entiers strictement positifs.")

  if (length(block_sizes) != length(nb_block))
    stop("'block_sizes' et 'nb_block' doivent avoir la même longueur.")

  if (any(block_sizes %% sum(ratio) != 0))
    stop("Chaque taille de bloc doit être divisible par sum(ratio) = ", sum(ratio), ".")

  if (length(ratio) != k)
    stop("'ratio' doit être un vecteur de longueur k.")

  if (length(arm_label) != k)
    stop("'arm_label' doit avoir exactement k éléments.")

  if (!is.numeric(arm_code) || length(arm_code) != k)
    stop("'arm_code' doit être un vecteur numérique de longueur k.")

  if (!is.null(strat_vars)) {
    if (!is.list(strat_vars))
      stop("'strat_vars' doit être une liste.")
    for (var in names(strat_vars)) {
      if (!all(c("codes", "labels") %in% names(strat_vars[[var]])))
        stop("La variable '", var, "' doit contenir 'codes' et 'labels'.")
      if (length(strat_vars[[var]]$codes) != length(strat_vars[[var]]$labels))
        stop("'codes' et 'labels' de '", var, "' doivent avoir la même longueur.")
    }
  }

  # --- Stockage randomisation ---
  .trialdesign_env$nom_etude   <- nom_etude
  .trialdesign_env$circuit     <- circuit
  .trialdesign_env$k           <- k
  .trialdesign_env$block_sizes <- block_sizes
  .trialdesign_env$nb_block    <- nb_block
  .trialdesign_env$ratio       <- ratio
  .trialdesign_env$arm_label   <- arm_label
  .trialdesign_env$arm_code    <- arm_code
  .trialdesign_env$strat_vars  <- strat_vars

  # --- Stockage page de garde ---
  .trialdesign_env$id_etude      <- id_etude
  .trialdesign_env$libelle_etude <- libelle_etude # Stockage du libellé
  .trialdesign_env$investigateur <- investigateur
  .trialdesign_env$methodologiste <- methodologiste
  .trialdesign_env$biostatisticien <- biostatisticien
  .trialdesign_env$confidentiel  <- confidentiel

  # --- NOUVEAUX CHAMPS USMR ---
  .trialdesign_env$code_usmr       <- code_usmr
  .trialdesign_env$indice_document <- indice_document

  # --- Message ---
  message("✔ Essai initialisé : ", nom_etude)
  message("  Circuit      : ", circuit)

  invisible(NULL)
}
