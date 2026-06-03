#' FONCTION init_essai()
#'
#' @description
#' D\u00e9finit et valide les param\u00e8tres de l'essai clinique, puis les retourne
#' sous forme d'un objet liste. Cet objet doit \u00eatre assign\u00e9 et pass\u00e9 en
#' argument \u00e0 rand() et corresp().
#'
#' @param nom_etude Caract\u00e8re. Nom de l'\u00e9tude (utilis\u00e9 dans les noms de fichiers).
#' @param circuit Caract\u00e8re. Circuit de randomisation : \code{"ennov"} ou \code{"redcap"}.
#' @param k Entier. Nombre de groupes de traitement (>= 2).
#' @param block_sizes Vecteur num\u00e9rique. Tailles r\u00e9elles des blocs.
#' @param nb_block Vecteur num\u00e9rique. Nombre de blocs souhait\u00e9s pour chaque taille.
#' @param ratio Vecteur num\u00e9rique de longueur k. Ratio d'allocation.
#'   Si NULL, un ratio \u00e9quilibr\u00e9 1:1:...:1 est utilis\u00e9.
#' @param arm_label Vecteur de caract\u00e8res de longueur k. Libell\u00e9s des groupes.
#'   Si NULL, "Groupe1", "Groupe2"... sont utilis\u00e9s.
#' @param arm_code Vecteur num\u00e9rique de longueur k. Codes des groupes.
#'   Si NULL, 1, 2, ..., k sont utilis\u00e9s.
#' @param strat_vars Liste optionnelle d\u00e9crivant les variables de stratification.
#'   Chaque \u00e9l\u00e9ment est une liste avec \code{codes} (vecteur num\u00e9rique) et
#'   \code{labels} (vecteur de caract\u00e8res).
#'   Exemple : \code{list(sexe = list(codes = c(1,2), labels = c("Homme","Femme")))}.
#' @param id_etude Caract\u00e8re. Identifiant officiel de l'\u00e9tude. Ex : "CHUBX2024/01".
#'   Par d\u00e9faut NULL.
#' @param libelle_etude Caract\u00e8re. Libell\u00e9 complet de l'\u00e9tude. Par d\u00e9faut NULL.
#' @param investigateur Caract\u00e8re. Nom de l'investigateur coordinateur.
#'   Par d\u00e9faut NULL.
#' @param methodologiste Caract\u00e8re. Nom du m\u00e9thodologiste. Par d\u00e9faut NULL.
#' @param biostatisticien Caract\u00e8re. Nom du biostatisticien. Par d\u00e9faut NULL.
#' @param confidentiel Logique. Si TRUE, affiche la mention CONFIDENTIEL sur
#'   la page de garde. Par d\u00e9faut FALSE.
#' @param code_usmr Caract\u00e8re. Code du document USMR. Par d\u00e9faut NULL.
#' @param indice_document Caract\u00e8re. Indice du document. Par d\u00e9faut NULL.
#'
#' @return Une liste contenant tous les param\u00e8tres valid\u00e9s de l'essai.
#'   Doit \u00eatre assign\u00e9e : \code{essai <- init_essai(...)}.
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

  # --- V\u00e9rification circuit ---
  if (!circuit %in% c("ennov", "redcap")) {
    stop("'circuit' doit \u00eatre 'ennov' ou 'redcap'.")
  }

  # --- Valeurs par d\u00e9faut ---
  if (is.null(ratio))     ratio     <- rep(1, k)
  if (is.null(arm_label)) arm_label <- paste0("Groupe", 1:k)
  if (is.null(arm_code))  arm_code  <- 1:k

  # --- V\u00e9rifications arguments randomisation ---
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
    # Param\u00e8tres randomisation
    nom_etude   = nom_etude,
    circuit     = circuit,
    k           = k,
    block_sizes = block_sizes,
    nb_block    = nb_block,
    ratio       = ratio,
    arm_label   = arm_label,
    arm_code    = arm_code,
    strat_vars  = strat_vars,
    # Param\u00e8tres page de garde
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
