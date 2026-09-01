#' Initialize a clinical trial design
#'
#' @description
#' Defines and validates the main parameters of a clinical trial and returns
#' them as a list. The resulting object is intended to be passed to
#' \code{\link{rand}} and \code{\link{corresp}} to generate the randomization
#' and treatment allocation lists.
#'
#' @param nom_etude Character string. Study acronym, used in generated file names.
#' @param circuit Character string. Data management system used: \code{"ennov"} or \code{"redcap"}.
#' @param k Integer. Number of treatment groups. Must be greater than or equal to 2. Defaults to \code{2}.
#' @param block_sizes Integer vector. Sizes of the randomization blocks.
#' @param nb_block Integer vector. Number of blocks to generate for each corresponding block size.
#' @param ratio Numeric vector of length \code{k}. Allocation ratio between treatment groups. If \code{NULL}, an equal allocation ratio
#' (\code{1:1:...:1}) is used.
#' @param arm_label Character vector of length \code{k}. Labels of the treatment groups. If \code{NULL}, \code{"Groupe1"}, \code{"Groupe2"},
#' ..., are used.
#' @param arm_code Integer vector of length \code{k}. Codes assigned to the treatment groups. If \code{NULL}, \code{1, 2, ..., k} are used.
#' @param strat_vars Optional list describing the stratification variables.
#'  Each element must be a list containing \code{codes}, a numeric vector, and \code{labels}, a character vector.
#'  For example:
#'  \code{list(sex = list(codes = c(1, 2), labels = c("Male", "Female")))}.
#' @param id_etude Character string. Official study identifier, for example: \code{"CHUBX2024/01"}. Defaults to \code{NULL}.
#' @param libelle_etude Character string. Full study title. Defaults to \code{NULL}.
#' @param investigateur Character string. Name of the principal investigator. Defaults to \code{NULL}.
#' @param methodologiste Character string. Name of the methodologist. Defaults to \code{NULL}.
#' @param biostatisticien Character string. Name of the biostatistician. Defaults to \code{NULL}.
#' @param confidentiel Logical. If \code{TRUE}, displays a \code{"CONFIDENTIAL"} notice on the cover page. Defaults to \code{FALSE}.
#' @param indice_document Character string. Document version or index. Defaults to \code{"02"}.
#'
#' @return A list containing the validated trial parameters. The returned object should
#' typically be assigned to an object, for example: \code{essai <- init_essai(...)}.
#'
#' @examples
#' \dontrun{
#' essai <- init_essai(
#'   nom_etude     = "ESSAI_CLINIQUE",
#'   circuit       = "ennov",
#'   k             = 2,
#'   block_sizes   = c(4, 6),
#'   nb_block      = c(10, 10),
#'   arm_label     = c("1 - Traitement", "2 - Placebo"),
#'   arm_code      = c(1,2),
#'   strat_vars    = list(
#'     sexe   = list(codes = c(1, 2), labels = c("Femme", "Homme")),
#'     centre = list(codes = c(1, 2), labels = c("Centre1", "Centre2"))
#'   ),
#'   id_etude      = "CHUBX2026-000",
#'   libelle_etude = "Full study title"
#' )
#' rand(essai, seed = 42, statut = "FICTIVE", version = "V01")
#' }
#' @export

init_essai <- function(nom_etude,
                       circuit,
                       k                 = 2,
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
                       indice_document   = "02") {

  # --- Verification circuit ---
  if (!circuit %in% c("ennov", "redcap")) {
    stop("'circuit' doit etre 'ennov' ou 'redcap'.")
  }

  if (circuit == "ennov" && is.null(strat_vars)) {
    stop("Une variable de stratification est obligatoire pour le circuit 'ennov'. ",
         "Fournissez 'strat_vars' (ex : strat_vars = list(centre = list(codes = c(1,2), labels = c(\"Centre 1\",\"Centre 2\")))).")
  }

  # --- Vérification de k ---
  if (!is.numeric(k) || k < 2 || k != round(k))
    stop("'k' doit etre un entier >= 2.")

  # --- Valeurs par defaut ---
  if (is.null(ratio))     ratio     <- rep(1, k)
  if (is.null(arm_label)) arm_label <- paste0("Groupe", 1:k)
  if (is.null(arm_code))  arm_code  <- 1:k

  # --- Verifications arguments randomisation ---
  if (!is.numeric(block_sizes) || any(block_sizes <= 0) || any(block_sizes != round(block_sizes)))
    stop("'block_sizes' doit contenir des entiers strictement positifs.")

  if (!is.numeric(nb_block) || any(nb_block <= 0) || any(nb_block != round(nb_block)))
    stop("'nb_block' doit contenir des entiers strictement positifs.")

  if (length(block_sizes) != length(nb_block))
    stop("'block_sizes' et 'nb_block' doivent avoir la meme longueur.")

  if (length(ratio) != k)
    stop("'ratio' doit etre un vecteur de longueur k.")

  if (any(block_sizes %% sum(ratio) != 0))
    stop("Chaque taille de bloc doit etre divisible par sum(ratio) = ", sum(ratio), ".")

  if (length(arm_label) != k)
    stop("'arm_label' doit avoir exactement k elements.")

  if (!is.numeric(arm_code) || length(arm_code) != k)
    stop("'arm_code' doit etre un vecteur numerique de longueur k.")

  if (!is.null(strat_vars)) {
    if (!is.list(strat_vars))
      stop("'strat_vars' doit etre une liste.")
    for (var in names(strat_vars)) {
      if (!all(c("codes", "labels") %in% names(strat_vars[[var]])))
        stop("La variable '", var, "' doit contenir 'codes' et 'labels'.")
      if (length(strat_vars[[var]]$codes) != length(strat_vars[[var]]$labels))
        stop("'codes' et 'labels' de '", var, "' doivent avoir la meme longueur.")
      if (any(duplicated(strat_vars[[var]]$codes)))
        stop("Les 'codes' de '", var, "' doivent etre uniques (doublon detecte).")
    }
  }

  # --- Construction de l'objet essai ---
  essai <- list(
    # Parametres randomisation
    nom_etude   = nom_etude,
    circuit     = circuit,
    k           = k,
    block_sizes = block_sizes,
    nb_block    = nb_block,
    ratio       = ratio,
    arm_label   = arm_label,
    arm_code    = arm_code,
    strat_vars  = strat_vars,
    # Parametres page de garde
    id_etude        = id_etude,
    libelle_etude   = libelle_etude,
    investigateur   = investigateur,
    methodologiste  = methodologiste,
    biostatisticien = biostatisticien,
    confidentiel    = confidentiel,
    indice_document = indice_document
  )

  # --- Message de confirmation ---
  message("\u2714 Essai initialise : ", nom_etude)
  message("  Circuit      : ", circuit)
  message("  N/strate     : ", sum(block_sizes * nb_block))
  if (!is.null(strat_vars)) {
    n_strates <- prod(sapply(strat_vars, function(v) length(v$codes)))
    message("  Strates      : ", n_strates)
  }

  return(essai)
}
