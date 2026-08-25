#' Calculate sample size for a single-arm phase II trial
#'
#' @description
#' Calculates the required sample size for a single-arm phase II trial using
#' A'Hern's or Fleming's method. These methods are used to assess whether an
#' observed success rate is sufficiently promising to justify further
#' investigation in a subsequent phase III trial.
#'
#' @param p0 Numeric vector. Success proportion under the null hypothesis, corresponding to an unacceptable response rate.
#' @param p1 Numeric vector. Expected success proportion under the alternative hypothesis.
#' Very small differences between \code{p1} and \code{p0} may result in long computation times, particularly when
#' \code{method = "fleming"}.
#' @param alpha Numeric vector. Type I error rate. Defaults to \code{0.05}.
#' @param power Numeric vector. Desired statistical power. Defaults to \code{0.80}.
#' @param method Character string. Method used for the sample size calculation:
#' \itemize{
#' \item \code{"ahern"}: Exact method based on the binomial distribution
#' (A'Hern, 2001). This method performs an exhaustive search using
#' \code{\link[stats]{pbinom}} and generally results in larger sample sizes.
#' \item \code{"fleming"}: Normal approximation to the binomial distribution (Fleming, 1982).
#' This method uses a closed-form formula and generally results in smaller sample sizes
#' than A'Hern's method, but may be less reliable for small samples (fewer than 50 subjects).
#' }
#' @param missing_prop Numeric vector. Proportion of missing data. Defaults to \code{0}.
#' Used to adjust the required number of patients.
#' @param nmax Numeric. Maximum sample size considered by the exhaustive search
#' used with A'Hern's method. Defaults to \code{200}.
#'
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap map_dbl
#' @importFrom stats pbinom qnorm
#'
#' @return A data frame with one row for each combination of input parameters and the following columns:
#' \itemize{
#' \item \code{methode}: Sample size calculation method.
#' \item \code{p0}: Success proportion under the null hypothesis.
#' \item \code{p1}: Expected success proportion under the alternative
#' hypothesis.
#' \item \code{alpha}: Type I error rate.
#' \item \code{puissance}: Desired statistical power.
#' \item \code{missing_prop}: Proportion of missing data.
#' \item \code{n_patient}: Required number of patients before adjustment for missing data.
#' \item \code{n_ajuste}: Required number of patients after adjustment for missing data.
#' \item \code{n_succes}: Minimum number of successes required to declare
#'  the treatment sufficiently promising.
#' }

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
