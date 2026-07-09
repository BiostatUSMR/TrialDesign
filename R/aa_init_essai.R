#' FONCTION init_essai()
#'
#' @description
#' Definit et valide les parametres de l'essai clinique, puis les retourne
#' sous forme d'un objet liste. Cet objet doit etre assigne et passe en
#' argument a rand() et corresp().
#'
#' @param nom_etude Caractere. Nom de l'etude (utilise dans les noms de fichiers).
#' @param circuit Caractere. Circuit de randomisation : \code{"ennov"} ou \code{"redcap"}.
#' @param k Entier. Nombre de groupes de traitement (>= 2).
#' @param block_sizes Vecteur numerique. Tailles reelles des blocs.
#' @param nb_block Vecteur numerique. Nombre de blocs souhaites pour chaque taille.
#' @param ratio Vecteur numerique de longueur k. Ratio d'allocation. Si NULL, un ratio equilibre 1:1:...:1 est utilise.
#' @param arm_label Vecteur de caracteres de longueur k. Libelles des groupes. Si NULL, "Groupe1", "Groupe2"... sont utilises.
#' @param arm_code Vecteur numerique de longueur k. Codes des groupes. Si NULL, 1, 2, ..., k sont utilises.
#' @param strat_vars Liste optionnelle decrivant les variables de stratification.
#'   Chaque element est une liste avec \code{codes} (vecteur numerique) et \code{labels} (vecteur de caracteres).
#'   Exemple : \code{list(sexe = list(codes = c(1,2), labels = c("Homme","Femme")))}.
#' @param id_etude Caractere. Identifiant officiel de l'etude. Ex : "CHUBX2024/01". Par defaut NULL.
#' @param libelle_etude Caractere. Libelle complet de l'etude. Par defaut NULL.
#' @param investigateur Caractere. Nom de l'investigateur principal. Par defaut NULL.
#' @param methodologiste Caractere. Nom du methodologiste. Par defaut NULL.
#' @param biostatisticien Caractere. Nom du biostatisticien. Par defaut NULL.
#' @param confidentiel Logique. Si TRUE, affiche la mention CONFIDENTIEL sur la page de garde. Par defaut FALSE.
#' @param indice_document Caractere. Indice du document. Par defaut 02.
#'
#' @return Une liste contenant tous les parametres valides de l'essai.
#'   Doit etre assignee : \code{essai <- init_essai(...)}.
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
                       indice_document   = "02") {

  # --- Verification circuit ---
  if (!circuit %in% c("ennov", "redcap")) {
    stop("'circuit' doit etre 'ennov' ou 'redcap'.")
  }

  if (circuit == "ennov" && is.null(strat_vars)) {
    stop("Une variable de stratification est obligatoire pour le circuit 'ennov'. ",
         "Fournissez 'strat_vars' (ex : strat_vars = list(centre = list(codes = c(1,2), labels = c(\"Centre 1\",\"Centre 2\")))).")
  }

  # --- Valeurs par defaut ---
  if (is.null(ratio))     ratio     <- rep(1, k)
  if (is.null(arm_label)) arm_label <- paste0("Groupe", 1:k)
  if (is.null(arm_code))  arm_code  <- 1:k

  # --- Verifications arguments randomisation ---
  if (!is.numeric(k) || k < 2 || k != round(k))
    stop("'k' doit etre un entier >= 2.")

  if (!is.numeric(block_sizes) || any(block_sizes <= 0))
    stop("'block_sizes' doit contenir des entiers strictement positifs.")

  if (!is.numeric(nb_block) || any(nb_block <= 0))
    stop("'nb_block' doit contenir des entiers strictement positifs.")

  if (length(block_sizes) != length(nb_block))
    stop("'block_sizes' et 'nb_block' doivent avoir la meme longueur.")

  if (any(block_sizes %% sum(ratio) != 0))
    stop("Chaque taille de bloc doit etre divisible par sum(ratio) = ", sum(ratio), ".")

  if (length(ratio) != k)
    stop("'ratio' doit etre un vecteur de longueur k.")

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
