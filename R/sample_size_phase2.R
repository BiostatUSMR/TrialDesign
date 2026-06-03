

#' FONCTION sample_size_phase2
#'
#' @description
#' Calcule la taille d'échantillon nécessaire pour un essai de phase II à un bras
#' selon les méthodes de A'Hern ou de Fleming, pour tester si la proportion de succès
#' attendue est suffisamment élevée pour justifier un essai de phase III.
#'
#' @param p0 Proportion de succès sous l'hypothèse nulle (taux inacceptable)
#' @param p1 Proportion de succès sous l'hypothèse alternative (taux attendu)
#' @param alpha Niveau de signification (erreur de type I, défaut = 0.05)
#' @param power Puissance souhaitée (1 - erreur de type II, défaut = 0.80)
#' @param method Méthode de calcul ("ahern" ou "fleming")
#' @param missing_rate Proportion attendue de données manquantes (défaut = 0)
#' @param nmax Taille maximale testée pour A'Hern (défaut = 200)
#'
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap map_dbl
#' @importFrom clinfun ph2single
#' @importFrom stats pbinom
#'
#'
#' @return Un data.frame avec les colonnes : methode, p0, p1, alpha, power,
#'   missing_prop, n_patient, n_ajuste, n_succes.
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' sample_size_phase2(
#'   p0 = c(0.10,0.20),
#'   p1 = c(0.30,0.40),
#'   method = "ahern",
#'   missing_rate = 0.1,
#'   nmax = 50)
#'   }
#'
sample_size_phase2 <- function(
    p0,
    p1,
    alpha = 0.05,
    power = 0.80,
    method = c("ahern","fleming"),
    missing_rate = 0,
    nmax=200
){

  method <- match.arg(method)

  # Vérifications
  if(any(c(p0,p1) < 0 | c(p0,p1) > 1)) stop("p0 et p1 doivent être compris entre 0 et 1.")
  if(any(p0 >= p1)) stop("p0 doit être strictement inférieur à p1.")
  if(missing_rate < 0 | missing_rate >= 1) stop("missing_rate doit être compris entre 0 et 1.")

  params <- expand.grid(p0=p0, p1=p1, alpha=alpha, power=power, missing_prop=missing_rate)

  res <- dplyr::mutate(
    params,
    tmp = purrr::pmap(
      list(p0,p1,alpha,power),
      function(p0,p1,alpha,power){

        #-------- A'Hern --------
        if(method=="ahern"){
          # On cherche tous les couples n_patient, n_succes
          solution <- NULL
          for(n_patient in 10:nmax){
            for(n_succes in 0:n_patient){
              type1 <- 1 - stats::pbinom(n_succes-1,n_patient,p0)
              type2 <- 1 - stats::pbinom(n_succes-1,n_patient,p1)
              if(type1 <= alpha & type2 >= power){
                solution <- list(n_patient=n_patient,n_succes=n_succes)
                break  # On prend le premier n_succes valide pour ce n_patient
              }
            }
            if(!is.null(solution)) break
          }

          if(is.null(solution)){
            warning(paste("Aucune solution trouvée pour p0=",p0," p1=",p1," avec nmax=",nmax))
            return(list(n_patient=NA_real_, n_succes=NA_real_))
          } else return(solution)
        }

        #-------- Fleming --------
        if(method=="fleming"){
          res <- clinfun::ph2single(pu = p0, pa = p1, ep1 = alpha, ep2 = 1-power)
          return(list(n_patient=res$n[1], n_succes=res$r[1]))
        }
      }
    )
  )

  res <- dplyr::mutate(
    res,
    n_patient = purrr::map_dbl(tmp, ~ .x$n_patient),
    n_succes  = purrr::map_dbl(tmp, ~ .x$n_succes),
    n_ajuste  = ceiling(n_patient/(1-missing_prop)),
    methode   = method
  )

  res <- dplyr::select(res,methode,p0,p1,alpha,power,missing_prop,n_patient,n_ajuste,n_succes)

  return(res)
}
