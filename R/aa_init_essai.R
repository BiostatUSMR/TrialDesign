#' FONCTION init_essai()
#'
#' @description
#' Définit et valide les paramètres de l'essai clinique, puis les retourne
#' sous forme d'un objet liste. Cet objet doit être assigné et passé en
#' argument à rand() et corresp().
#'
#' @param nom_etude Caractère. Nom de l'étude (utilisé dans les noms de fichiers).
#' @param circuit Caractère. Circuit de randomisation : \code{"ennov"} ou \code{"redcap"}.
#' @param k Entier. Nombre de groupes de traitement (>= 2).
#' @param block_sizes Vecteur numérique. Tailles réelles des blocs.
#' @param nb_block Vecteur numérique. Nombre de blocs souhaités pour chaque taille.
#' @param ratio Vecteur numérique de longueur k. Ratio d'allocation. Si NULL, un ratio équilibré 1:1:...:1 est utilisé.
#' @param arm_label Vecteur de caractères de longueur k. Libellés des groupes. Si NULL, "Groupe1", "Groupe2"... sont utilisés.
#' @param arm_code Vecteur numérique de longueur k. Codes des groupes. Si NULL, 1, 2, ..., k sont utilisés.
#' @param strat_vars Liste optionnelle décrivant les variables de stratification.
#'   Chaque élément est une liste avec \code{codes} (vecteur numérique) et \code{labels} (vecteur de caractères).
#'   Exemple : \code{list(sexe = list(codes = c(1,2), labels = c("Homme","Femme")))}.
#' @param id_etude Caractère. Identifiant officiel de l'étude. Ex : "CHUBX2024/01". Par défaut NULL.
#' @param libelle_etude Caractère. Libellé complet de l'étude. Par défaut NULL.
#' @param investigateur Caractère. Nom de l'investigateur principal. Par défaut NULL.
#' @param methodologiste Caractère. Nom du méthodologiste. Par défaut NULL.
#' @param biostatisticien Caractère. Nom du biostatisticien. Par défaut NULL.
#' @param confidentiel Logique. Si TRUE, affiche la mention CONFIDENTIEL sur la page de garde. Par défaut FALSE.
#' @param code_usmr Caractère. Code du document USMR. Par défaut NULL.
#' @param indice_document Caractère. Indice du document. Par défaut NULL.
#'
#' @return Une liste contenant tous les paramètres validés de l'essai.
#'   Doit être assignée : \code{essai <- init_essai(...)}.
#'
#' @examples
#' \dontrun{
#' essai <- init_essai(
#'   nom_etude   = "ESSAI_CLINIQUE",
#'   circuit     = "ennov",
#'   k           = 2,
#'   block_sizes = c(4, 6),
#'   nb_block    = c(10, 10),
#'   arm_label   = c("Traitement", "Placebo"),
#'   strat_vars  = list(
#'     sexe   = list(codes = c(1, 2), labels = c("Femme", "Homme")),
#'     centre = list(codes = c(1, 2), labels = c("Centre1", "Centre2"))
#'   )
#' )
#' rand(essai, seed = 42, statut = "FICTIVE", version = "v01")
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

  # --- Construction de l'objet essai ---
  essai <- list(
    # Paramètres randomisation
    nom_etude   = nom_etude,
    circuit     = circuit,
    k           = k,
    block_sizes = block_sizes,
    nb_block    = nb_block,
    ratio       = ratio,
    arm_label   = arm_label,
    arm_code    = arm_code,
    strat_vars  = strat_vars,
    # Paramètres page de garde
    id_etude        = id_etude,
    libelle_etude   = libelle_etude,
    investigateur   = investigateur,
    methodologiste  = methodologiste,
    biostatisticien = biostatisticien,
    confidentiel    = confidentiel,
    code_usmr       = code_usmr,
    indice_document = indice_document
  )

  # --- Message de confirmation ---
  message("\u2714 Essai initialis\u00e9 : ", nom_etude)
  message("  Circuit      : ", circuit)
  message("  N/strate     : ", sum(block_sizes * nb_block))
  if (!is.null(strat_vars)) {
    n_strates <- prod(sapply(strat_vars, function(v) length(v$codes)))
    message("  Strates      : ", n_strates)
  }

  return(essai)
}
