#' FONCTION init_essai()
#'
#' @description
#' Définit, initialise et stocke les paramètres de l'essai clinique dans l'environnement interne
#' du package. Doit être appelée avant toute génération de liste.
#'
#'
#' @param nom_etude Caractère. Nom de l'étude .
#' @param circuit Caractère. Circuit de randomisation : "ennov" "redcap".
#' @param k Entier. Nombre de groupes de traitement (>= 2).
#' @param block_sizes Vecteur numérique. Tailles réelles des blocs.
#' @param nb_block Vecteur numérique. Nombre de blocs souhaités pour chaque taille.
#' @param ratio Vecteur numérique de longueur k. Ratio d'allocation pour chaque bloc.
#'   Si NULL, un ratio équilibré 1:1:...:1 est utilisé..
#' @param arm_label Vecteur de caractères de longueur k. Libellés des groupes de traitement.
#'   Si NULL, "Groupe1", "Groupe2"... sont utilisés.
#' @param arm_code Vecteur numérique de longueur k. Codes des groupes de traitement.
#'   Si NULL, 1, 2, ..., k sont utilisés..
#' @param strat_vars Liste optionnelle décrivant les variables de stratification.
#'   Chaque élément est une liste avec:
#'   codes (vecteur numérique) et labels (vecteur de caractères).
#'   Exemple : list(sexe = list(codes = c(1,2), labels = c("Homme","Femme"))).
#' @param id_etude Caractère . Identifiant officiel de l'étude.
#'   Ex : "CHUBX2024/01". Par défaut NULL.
#' @param libelle_etude Caractère . Libellé complet de l'étude.
#'   Par défaut NULL.
#' @param investigateur Caractère. Nom de l'investigateur coordinateur.
#'   Par défaut NULL.
#' @param methodologiste Caractère . Nom du méthodologiste.
#'   Par défaut NULL.
#' @param biostatisticien Caractère . Nom du biostatisticien.
#'   Par défaut NULL.
#' @param confidentiel Logique. Si TRUE, affiche la mention CONFIDENTIEL sur
#'   la page de garde. Par défaut FALSE.
#' @param code_usmr Caractère . Code du document USMR.
#'    Par défaut NULL.
#' @param indice_document Caractère . Indice du document.
#'   Par défaut NULL.
#'
#' @return Invisible. Stocke les paramètres dans l'environnement interne.
#'
#' @examples
#' \dontrun{
#' init_essai(
#'   nom_etude   = "ESSAI_CLINIQUE",
#'   circuit     = "ennov",
#'   k           = 2,
#'   block_sizes = c(4,6),
#'   nb_block    = c(10,10),
#'   arm_label   = c("Traitement", "Placebo"),
#'   strat_vars  = list(
#'     sexe   = list(codes = c(1, 2), labels = c("Femme", "Homme")),
#'     centre = list(codes = c(1, 2), labels = c("Centre1", "Centre2"))
#'   )
#' )
#' }
#' @export


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
    stop("'circuit' doit \u00eatre 'ennov' ou 'redcap'.")
  }

  # --- Valeurs par défaut ---
  if (is.null(ratio))     ratio     <- rep(1, k)
  if (is.null(arm_label)) arm_label <- paste0("Groupe", 1:k)
  if (is.null(arm_code))  arm_code  <- 1:k

  # --- Vérifications arguments randomisation ---
  if (!is.numeric(k) || k < 2 || k != round(k))
    stop("'k' doit \u00eatre un entier >= 2.")

  if (!is.numeric(block_sizes) || any(block_sizes <= 0))
    stop("'block_sizes' doit contenir des entiers strictement positifs.")

  if (!is.numeric(nb_block) || any(nb_block <= 0))
    stop("'nb_block' doit contenir des entiers strictement positifs.")

  if (length(block_sizes) != length(nb_block))
    stop("'block_sizes' et 'nb_block' doivent avoir la m\u00eame longueur.")

  if (any(block_sizes %% sum(ratio) != 0))
    stop("Chaque taille de bloc doit \u00eatre divisible par sum(ratio) = ", sum(ratio), ".")

  if (length(ratio) != k)
    stop("'ratio' doit \u00eatre un vecteur de longueur k.")

  if (length(arm_label) != k)
    stop("'arm_label' doit avoir exactement k \u00e9l\u00e9ments.")

  if (!is.numeric(arm_code) || length(arm_code) != k)
    stop("'arm_code' doit \u00eatre un vecteur num\u00e9rique de longueur k.")

  if (!is.null(strat_vars)) {
    if (!is.list(strat_vars))
      stop("'strat_vars' doit \u00eatre une liste.")
    for (var in names(strat_vars)) {
      if (!all(c("codes", "labels") %in% names(strat_vars[[var]])))
        stop("La variable '", var, "' doit contenir 'codes' et 'labels'.")
      if (length(strat_vars[[var]]$codes) != length(strat_vars[[var]]$labels))
        stop("'codes' et 'labels' de '", var, "' doivent avoir la m\u00eame longueur.")
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
  .trialdesign_env$libelle_etude <- libelle_etude # Stockage du libell\u00e9
  .trialdesign_env$investigateur <- investigateur
  .trialdesign_env$methodologiste <- methodologiste
  .trialdesign_env$biostatisticien <- biostatisticien
  .trialdesign_env$confidentiel  <- confidentiel

  # --- NOUVEAUX CHAMPS USMR ---
  .trialdesign_env$code_usmr       <- code_usmr
  .trialdesign_env$indice_document <- indice_document

  # --- Message ---
  message("\u2714 Essai initialis\u00e9 : ", nom_etude)
  message("  Circuit      : ", circuit)

  invisible(NULL)
}
