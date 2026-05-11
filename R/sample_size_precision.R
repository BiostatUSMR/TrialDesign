#' Calcul de la taille d'\u00e9chantillon par approche de pr\u00e9cision
#'
#' @description
#' Calcule la taille d'\u00e9chantillon n\u00e9cessaire pour estimer avec une pr\u00e9cision
#' donn\u00e9e une proportion, une sensibilit\u00e9, une sp\u00e9cificit\u00e9, ou simultan\u00e9ment
#' la sensibilit\u00e9 et la sp\u00e9cificit\u00e9 d'un test diagnostique.
#' Le calcul repose sur la m\u00e9thode de Wilson via le package \pkg{presize}.
#'
#' @param p Vecteur num\u00e9rique. Proportion(s) \u00e0 estimer, entre 0 et 1.
#'   Mutuellement exclusif avec \code{sens} et \code{spec}.
#' @param prev Vecteur num\u00e9rique. Pr\u00e9valence de la maladie dans la population,
#'   entre 0 et 1. Requis si \code{sens} ou \code{spec} est sp\u00e9cifi\u00e9.
#' @param sens Vecteur num\u00e9rique. Sensibilit\u00e9 attendue du test diagnostique,
#'   entre 0 et 1.
#' @param spec Vecteur num\u00e9rique. Sp\u00e9cificit\u00e9 attendue du test diagnostique,
#'   entre 0 et 1.
#' @param precision Vecteur num\u00e9rique. Demi-largeur souhait\u00e9e de l'intervalle
#'   de confiance. Par d\u00e9faut \code{0.05}.
#' @param conf.level Num\u00e9rique. Niveau de confiance de l'intervalle.
#'   Par d\u00e9faut \code{0.95}.
#' @param missing_rate Vecteur num\u00e9rique. Taux de donn\u00e9es manquantes attendu,
#'   entre 0 et 1 (exclu). Par d\u00e9faut \code{0}.
#'
#' @return Un data.frame dont les colonnes varient selon le type d'estimation :
#'   \itemize{
#'     \item Proportion : \code{p}, \code{precision}, \code{conf.level},
#'       \code{missing_prop}, \code{n_prop}, \code{n_prop_ajuste}
#'     \item Sensibilit\u00e9 seule : \code{sens}, \code{prev}, \code{precision},
#'       \code{conf.level}, \code{missing_prop}, \code{n_sens}, \code{n_sens_ajuste}
#'     \item Sp\u00e9cificit\u00e9 seule : \code{spec}, \code{prev}, \code{precision},
#'       \code{conf.level}, \code{missing_prop}, \code{n_spec}, \code{n_spec_ajuste}
#'     \item Sensibilit\u00e9 + Sp\u00e9cificit\u00e9 : toutes les colonnes ci-dessus combin\u00e9es
#'   }
#'   Les effectifs sont syst\u00e9matiquement arrondis \u00e0 l'entier sup\u00e9rieur.
#'
#' @details
#' La largeur totale de l'intervalle de confiance utilis\u00e9e dans les calculs
#' est \code{2 * precision}. Les arguments \code{p} et \code{sens}/\code{spec}
#' sont mutuellement exclusifs. La pr\u00e9valence (\code{prev}) est obligatoire
#' pour le calcul de la sensibilit\u00e9 et de la sp\u00e9cificit\u00e9 car elle d\u00e9termine
#' le nombre de cas positifs et n\u00e9gatifs n\u00e9cessaires.
#'
#' @examples
#' \dontrun{
#' # Estimation d'une proportion
#' sample_size_precision(
#'   p = c(0.10, 0.20, 0.30),
#'   missing_rate = c(0.10, 0.20)
#' )
#'
#' # Sensibilit\u00e9 et sp\u00e9cificit\u00e9 simultan\u00e9ment
#' sample_size_precision(
#'   prev = c(0.20, 0.30),
#'   sens = c(0.80, 0.85),
#'   spec = c(0.85, 0.90),
#'   missing_rate = 0.10
#' )
#'
#' # Sp\u00e9cificit\u00e9 seule
#' sample_size_precision(
#'   prev = c(0.20, 0.30),
#'   spec = c(0.80, 0.85),
#'   missing_rate = 0.10
#' )
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap map_dbl
#' @export

sample_size_precision <- function(
    p = NULL,
    prev = NULL,
    sens = NULL,
    spec = NULL,
    precision = 0.05,
    conf.level = 0.95,
    missing_rate = 0
){

  #--------------------------
  # Vérifications des paramètres
  #--------------------------
  check_prop <- function(x,name){
    if(!is.null(x)){
      if(any(x < 0 | x > 1, na.rm=TRUE)){
        stop(paste(name,"doit être compris entre 0 et 1"))
      }
    }
  }

  check_prop(p,"p")
  check_prop(prev,"prev")
  check_prop(sens,"sens")
  check_prop(spec,"spec")

  if(!is.null(p) & (!is.null(sens) | !is.null(spec))){
    stop("Les arguments 'p' et 'sens'/'spec' sont mutuellement exclusifs : spécifier soit une proportion simple ('p'), soit une sensibilité/spécificité ('sens', 'spec').")
  }

  if((!is.null(sens) | !is.null(spec)) & is.null(prev)){
    stop("Argument 'prev' requis lorsque 'sens' ou 'spec' est spécifié.")
  }

  if(any(missing_rate < 0 | missing_rate >= 1)){
    stop("missing_rate doit être compris entre 0 et 1.")
  }

  # Remplacement des NULL par NA
  if(is.null(p)) p <- NA
  if(is.null(prev)) prev <- NA
  if(is.null(sens)) sens <- NA
  if(is.null(spec)) spec <- NA

  #--------------------------
  # Expansion grid
  #--------------------------
  params <- expand.grid(
    p = p,
    prev = prev,
    sens = sens,
    spec = spec,
    precision = precision,
    conf.level = conf.level,
    missing_prop = missing_rate
  )

  #--------------------------
  # Calculs
  #--------------------------
  res <- dplyr::mutate(
    params,
    tmp = purrr::pmap(
      list(p, prev, sens, spec, precision, conf.level),
      function(p, prev, sens, spec, precision, conf.level){

        # Sensibilité + Specificité
        if(!is.na(sens) & !is.na(spec) & !is.na(prev)){
          n_sens <- presize::prec_sens(
            sens = sens,
            prev = prev,
            conf.level = conf.level,
            conf.width = 2*precision,
            method = "wilson"
          )$ntot

          n_spec <- presize::prec_spec(
            spec = spec,
            prev = prev,
            conf.level = conf.level,
            conf.width = 2*precision,
            method = "wilson"
          )$ntot

          return(list(n_sens=ceiling(n_sens), n_spec=ceiling(n_spec)))
        }

        # Sensibilté seule
        else if(!is.na(sens) & is.na(spec) & !is.na(prev)){
          n <- presize::prec_sens(
            sens = sens,
            prev = prev,
            conf.level = conf.level,
            conf.width = 2*precision,
            method = "wilson"
          )$ntot
          return(list(n_sens=ceiling(n)))
        }

        # Specificité seule
        else if(is.na(sens) & !is.na(spec) & !is.na(prev)){
          n <- presize::prec_spec(
            spec = spec,
            prev = prev,
            conf.level = conf.level,
            conf.width = 2*precision,
            method = "wilson"
          )$ntot
          return(list(n_spec=ceiling(n), n_sens=NA, n_prop=NA))
        }

        # Proportion
        else if(!is.na(p)){
          n <- presize::prec_prop(
            p = p,
            conf.level = conf.level,
            conf.width = 2*precision,
            method = "wilson"
          )$n
          return(list(n_prop=ceiling(n), n_sens=NA, n_spec=NA))
        }

        else return(list(n_prop=NA, n_sens=NA, n_spec=NA))
      }
    )
  )

  res <- dplyr::mutate(
    res,
    n_prop = purrr::map_dbl(tmp, ~ ifelse(is.null(.x$n_prop), NA, ceiling(.x$n_prop))),
    n_sens = purrr::map_dbl(tmp, ~ ifelse(is.null(.x$n_sens), NA, ceiling(.x$n_sens))),
    n_spec = purrr::map_dbl(tmp, ~ ifelse(is.null(.x$n_spec), NA, ceiling(.x$n_spec)))
  )

  res <- dplyr::mutate(
    res,
    n_prop_ajuste = ifelse(!is.na(n_prop), ceiling(n_prop/(1-missing_prop)), NA),
    n_sens_ajuste = ifelse(!is.na(n_sens), ceiling(n_sens/(1-missing_prop)), NA),
    n_spec_ajuste = ifelse(!is.na(n_spec), ceiling(n_spec/(1-missing_prop)), NA)
  )

  #--------------------------
  # Filtrage dynamique des colonnes
  #--------------------------
  if(!all(is.na(res$p))){
    res <- dplyr::select(res, p,precision,conf.level,missing_prop,n_prop,n_prop_ajuste)
  } else if(!all(is.na(res$sens)) & all(is.na(res$spec))){
    res <- dplyr::select(res, sens,prev,precision,conf.level,missing_prop,n_sens,n_sens_ajuste)
  } else if(all(is.na(res$sens)) & !all(is.na(res$spec))){
    res <- dplyr::select(res, spec,prev,precision,conf.level,missing_prop,n_spec,n_spec_ajuste)
  } else if(!all(is.na(res$sens)) & !all(is.na(res$spec))){
    res <- dplyr::select(res, sens,spec,prev,precision,conf.level,missing_prop,
                         n_sens,n_sens_ajuste,n_spec,n_spec_ajuste)
  }

  return(res)
}
