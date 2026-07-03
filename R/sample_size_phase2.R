#' FONCTION sample_size_phase2
#'
#' @description
#' Calcule le nombre de sujets necessaire pour un essai de phase II a un bras
#' selon les methodes de A'Hern ou de Fleming, pour tester si la proportion de succes
#' attendue est suffisamment elevee pour justifier un essai de phase III.
#'
#' @param p0 Numerique. Proportion de succes sous l'hypothese nulle (taux inacceptable). Peut etre un vecteur.
#' @param p1 Numerique. Proportion de succes sous l'hypothese alternative (taux attendu).  Peut etre un vecteur. Attention : un ecart p1 - p0 tres faible peut rendre le calcul
#'   tres long (recherche exhaustive), en particulier pour method = "fleming".
#' @param alpha Numerique. Risque de premiere espece. Par defaut = 0.05. Peut etre un vecteur
#' @param power Numerique. Puissance. Par defaut 0.80. Peut etre un vecteur.
#' @param method Caractere. Methode de calcul ("ahern" ou "fleming").
#' \itemize{
#'   \item \code{"ahern"} : Recherche exacte sur la loi binomiale (A'Hern, 2001).
#'     Plus precise mais generalement des effectifs plus eleves. Recherche exhaustive
#'     via \code{stats::pbinom()}.
#'   \item \code{"fleming"} : Approximation normale de la loi binomiale (Fleming, 1982).
#'     Calcul direct (formule fermee), effectifs generalement plus faibles que A'Hern,
#'     mais approximation moins fiable pour les petits echantillons (< 50 sujets).
#' }
#' @param missing_prop Numerique. Taux de donnees manquantes. Par defaut 0. Peut etre un vecteur. Utilise pour calculer n1_pdv, n2_pdv et n_total_pdv.
#' @param nmax Numerique. Taille maximale testee pour A'Hern. Par defaut = 200.
#'
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap map_dbl
#' @importFrom stats pbinom qnorm
#'
#' @return Un data.frame avec les colonnes : methode, p0, p1, alpha, puissance,
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
#'   missing_prop = 0.1,
#'   nmax = 50)
#'   }
#'
sample_size_phase2 <- function(
    p0,
    p1,
    alpha = 0.05,
    power = 0.80,
    method = c("ahern","fleming"),
    missing_prop = 0,
    nmax=200
){

  method <- match.arg(method)

  # ------------------------------------
  # Verifications generales
  # ------------------------------------
  if(any(c(p0,p1) < 0 | c(p0,p1) > 1))
    stop("p0 et p1 doivent etre compris entre 0 et 1.")

  if (any(alpha <= 0) | any(alpha >= 1))
    stop("'alpha' doit etre compris entre 0 et 1.")

  if (any(power <= 0) | any(power >= 1))
    stop("'power' doit etre compris entre 0 et 1.")

  if (any(missing_prop < 0) | any(missing_prop >= 1))
    stop("'missing_prop' doit etre compris entre 0 (inclus) et 1 (exclus).")

  if (!is.numeric(nmax) || length(nmax) != 1 || nmax < 10 || nmax != round(nmax))
    stop("'nmax' doit etre un entier scalaire >= 10.")

  # ------------------------------------
  # Grille de parametres
  # ------------------------------------
  params <- expand.grid(p0=p0, p1=p1, alpha=alpha, power=power, missing_prop=missing_prop)

  if (any(params$p0 >= params$p1))
    stop("Toutes les combinaisons p0/p1 doivent verifier p0 < p1.")

  # ------------------------------------
  # Calcul
  # ------------------------------------
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
            warning(paste("Aucune solution trouv\u00e9e pour p0=",p0," p1=",p1," avec nmax=",nmax))
            return(list(n_patient=NA_real_, n_succes=NA_real_))
          } else return(solution)
        }

        #-------- Fleming --------
        if(method=="fleming"){
          z_alpha <- stats::qnorm(1 - alpha)
          z_beta  <- stats::qnorm(power)   # power = 1 - beta, donc z_{1-beta} = qnorm(power)

          n_continu <- (z_alpha * sqrt(p0*(1-p0)) + z_beta * sqrt(p1*(1-p1)))^2 / (p1 - p0)^2
          n_patient <- ceiling(n_continu)

          r_continu <- n_patient * p0 + z_alpha * sqrt(n_patient * p0 * (1 - p0))
          n_succes  <- round(r_continu) + 1   # rejet si X >= n_succes (X > r)

          return(list(n_patient = n_patient, n_succes = n_succes))
        }
      }
    )
  )

  res <- dplyr::mutate(
    res,
    n_patient = purrr::map_dbl(tmp, ~ .x$n_patient),
    n_succes  = purrr::map_dbl(tmp, ~ .x$n_succes),
    n_ajuste  = ceiling(n_patient/(1-missing_prop)),
    puissance = power,
    methode   = method
  )

  res <- dplyr::select(res, methode, p0, p1, alpha, puissance, missing_prop, n_patient, n_ajuste, n_succes)

  # ------------------------------------
  # Labels
  # ------------------------------------
  labels <- list(
    methode      = "Methode",
    p0           = "p0 (H0)",
    p1           = "p1 (H1)",
    alpha        = "Alpha",
    puissance    = "Puissance",
    missing_prop = "% d.m",
    n_patient    = "N patients",
    n_ajuste     = "N - avec d.m",
    n_succes     = "Seuil de succes (r)"
  )
  res <- labelled::set_variable_labels(res, !!!labels)

  # ------------------------------------
  # Enregistrement
  # ------------------------------------
  attr(res, "ssdesignr_type") <- "phase2"
  return(res)
}
