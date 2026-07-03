#' FONCTION sample_size_precision
#'
#'@description
#'Calcule le nombre de sujets necessaire pour estimer avec une precision donnee :
#'- une proportion
#'- une sensibilite,
#'- une specificite,
#' - ou simultanement la sensibilite et la specificite d'un test diagnostique.
#'
#' @param p Numerique. Proportion a estimer (entre 0 et 1). Peut etre vecteur.
#' @param prev Numerique. Prevalence de la maladie dans la population etudiee. Peut etre vecteur.
#' @param sens Numerique. Sensibilite attendue du test diagnostique (entre 0 et 1). Peut etre vecteur.
#' @param spec Numerique. Specificite attendue du test diagnostique (entre 0 et 1). Peut etre vecteur.
#' @param precision Numerique. Demi-largeur souhaitee de l intervalle de confiance. Peut etre vecteur.
#' @param conf.level Numerique. Niveau de confiance de l intervalle de confiance. Peut etre vecteur.
#' @param missing_prop Numerique. Proportion attendue de donnees manquantes. Peut etre vecteur. Par defaut 0.
#'
#' @return Un data.frame dont les colonnes dependent du mode de calcul :
#'   proportion (n_prop, n_prop_ajuste), sensibilite (n_sens, n_sens_ajuste),
#'   specificite (n_spec, n_spec_ajuste), ou les deux simultanement.
#'
#' @export
#'
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap map_dbl
#' @importFrom presize prec_prop prec_sens prec_spec
#'
#' @examples
#' \dontrun{
#' sample_size_precision(p=c(0.10,0.15,0.20,0.25,0.30,0.35,0.40),
#' missing_prop =c(0.1,0.2))
#' }

sample_size_precision <- function(
    p = NULL,
    prev = NULL,
    sens = NULL,
    spec = NULL,
    precision = 0.05,
    conf.level = 0.95,
    missing_prop = 0
){

  # ------------------------------------
  # Verifications generales
  # ------------------------------------
  check_prop <- function(x,name){
    if(!is.null(x)){
      if(any(x < 0 | x > 1, na.rm=TRUE)){
        stop(paste(name,"doit \u00eatre compris entre 0 et 1"))
      }
    }
  }

  check_prop(p,"p")
  check_prop(prev,"prev")
  check_prop(sens,"sens")
  check_prop(spec,"spec")

  if(!is.null(p) & (!is.null(sens) | !is.null(spec)))
    stop("Les arguments 'p' et 'sens'/'spec' sont mutuellement exclusifs : sp\u00e9cifier soit une proportion simple ('p'), soit une sensibilit\u00e9/sp\u00e9cificit\u00e9 ('sens', 'spec').")

  if (is.null(p) & is.null(sens) & is.null(spec))
    stop("Vous devez fournir au moins un des arguments 'p', 'sens' ou 'spec'.")

  if (!is.null(p) & !is.null(prev))
    warning("'prev' est ignore lorsque 'p' est specifie (utilise uniquement pour 'sens'/'spec').")

  if((!is.null(sens) | !is.null(spec)) & is.null(prev))
    stop("Argument 'prev' requis lorsque 'sens' ou 'spec' est sp\u00e9cifi\u00e9.")

  if (any(precision <= 0 | precision >= 0.5))
    stop("'precision' doit etre strictement positive et inferieure a 0.5 (demi-largeur d'un intervalle borne entre 0 et 1).")

  if (any(conf.level <= 0 | conf.level >= 1))
    stop("'conf.level' doit etre compris entre 0 et 1.")

  if(any(missing_prop < 0 | missing_prop >= 1))
    stop("missing_prop doit \u00eatre compris entre 0 et 1.")

  # Remplacement des NULL par NA
  if(is.null(p)) p <- NA
  if(is.null(prev)) prev <- NA
  if(is.null(sens)) sens <- NA
  if(is.null(spec)) spec <- NA

  # ------------------------------------
  # Grille de parametres
  # ------------------------------------
  params <- expand.grid(
    p = p,
    prev = prev,
    sens = sens,
    spec = spec,
    precision = precision,
    conf.level = conf.level,
    missing_prop = missing_prop
  )

  #--------------------------
  # Calculs
  #--------------------------
  res <- dplyr::mutate(
    params,
    tmp = purrr::pmap(
      list(p, prev, sens, spec, precision, conf.level),
      function(p, prev, sens, spec, precision, conf.level){

        # Sensibilite + Specificite
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

        # Sensibilte seule
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

        # Specificite seule
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
  # Labels
  #--------------------------
  labels_communs <- list(
    precision     = "Precision (demi-largeur IC)",
    conf.level    = "Niveau de confiance",
    missing_prop  = "% d.m",
    prev          = "Prevalence"
  )

  if(!all(is.na(res$p))){
    res <- dplyr::select(res, p, precision, conf.level, missing_prop, n_prop, n_prop_ajuste)
    labs <- c(labels_communs, list(p = "Proportion", n_prop = "N", n_prop_ajuste = "N - avec d.m"))
    type_res <- "precision_prop"

  } else if(!all(is.na(res$sens)) & all(is.na(res$spec))){
    res <- dplyr::select(res, sens, prev, precision, conf.level, missing_prop, n_sens, n_sens_ajuste)
    labs <- c(labels_communs, list(sens = "Sensibilite", n_sens = "N", n_sens_ajuste = "N - avec d.m"))
    type_res <- "precision_sens"

  } else if(all(is.na(res$sens)) & !all(is.na(res$spec))){
    res <- dplyr::select(res, spec, prev, precision, conf.level, missing_prop, n_spec, n_spec_ajuste)
    labs <- c(labels_communs, list(spec = "Specificite", n_spec = "N", n_spec_ajuste = "N - avec d.m"))
    type_res <- "precision_spec"

  } else if(!all(is.na(res$sens)) & !all(is.na(res$spec))){
    res <- dplyr::select(res, sens, spec, prev, precision, conf.level, missing_prop,
                         n_sens, n_sens_ajuste, n_spec, n_spec_ajuste)
    labs <- c(labels_communs, list(
      sens = "Sensibilite", n_sens = "N (sens.)", n_sens_ajuste = "N - avec d.m (sens.)",
      spec = "Specificite", n_spec = "N (spec.)", n_spec_ajuste = "N - avec d.m (spec.)"
    ))
    type_res <- "precision_sens_spec"
  }

  labs <- labs[names(labs) %in% names(res)]
  res <- labelled::set_variable_labels(res, !!!labs)

  # ------------------------------------
  # Enregistrement
  # ------------------------------------
  attr(res, "ssdesignr_type") <- type_res
  return(res)
}

