#' FONCTION sample_size_precision
#'
#'@description
#'Calcule la taille d'échantillon nécessaire pour estimer avec une précision donnée :
#'- une proportion
#'- une sensibilité,
#'- une spécificité,
#' - ou simultanément la sensibilité et la spécificité d'un test diagnostique.
#'
#'
#'
#' @param p proportion à estimer (numérique entre 0 et 1)
#' @param prev prévalence de la maladie dans la population étudiée
#' @param sens sensibilité attendue du test diagnostique (entre 0 et 1)
#' @param spec spécificité attendue du test diagnostique (entre 0 et 1)
#' @param precision demi-largeur souhaitée de l’intervalle de confiance
#' @param conf.level niveau de confiance de l’intervalle de confiance
#' @param missing_rate proportion attendue de données manquantes
#'
#' @return Un data.frame dont les colonnes dependent du mode de calcul :
#'   proportion (n_prop, n_prop_ajuste), sensibilite (n_sens, n_sens_ajuste),
#'   specificite (n_spec, n_spec_ajuste), ou les deux simultanement.
#'
#' @export
#'
#' @importFrom presize prec_prop prec_sens prec_spec
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap map_dbl
#'
#' @examples
#' \dontrun{
#' sample_size_precision(p=c(0.10,0.15,0.20,0.25,0.30,0.35,0.40),
#' missing_rate =c(0.1,0.2))
#' }

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

