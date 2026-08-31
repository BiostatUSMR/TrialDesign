#' Calculate sample size for a non-inferiority trial with a binary endpoint
#'
#' @description
#' Calculates the required sample size to demonstrate non-inferiority between
#' two treatment groups for a binary endpoint.
#'
#' Hypotheses:
#' \itemize{
#' \item \eqn{H_0: p_1 - p_2 \leq -\Delta}, corresponding to inferiority.
#' \item \eqn{H_1: p_1 - p_2 > -\Delta}, corresponding to non-inferiority.
#' }
#'
#' @note Only the chi-squared test is currently available for non-inferiority calculations.
#'
#' @param p1 Numeric vector. Expected proportion in group 1 (treatment group), ranging from 0 to 1.
#' @param p2 Numeric vector. Expected proportion in group 2 (control group), ranging from 0 to 1.
#' @param marge Numeric vector. Non-inferiority margin. Must be strictly positive.
#' @param power Numeric vector. Desired statistical power. Defaults to \code{0.80}.
#' @param alpha Numeric vector. Type I error rate. Defaults to \code{0.025}.
#' @param kappa Numeric. Allocation ratio \code{n1 / n2}. Defaults to \code{1}, corresponding to equal group sizes.
#' @param missing_prop Numeric vector. Proportion of missing data. Defaults to \code{0}.
#' @param sided Integer. Number of sides of the test: \code{1} for a one-sided test or
#' \code{2} for a two-sided test. Defaults to \code{1}.
#' @param choice Character string. The only accepted value is \code{"khi2"}.
#' This argument is retained for consistency with the other sample size functions in the package.
#'
#' @return A data frame with one row for each combination of input parameters and the
#' following columns:
#' \itemize{
#' \item \code{test}: Statistical method used (\code{"khi2"}).
#' \item \code{p1}, \code{p2}: Expected proportions in groups 1 and 2.
#' \item \code{marge}: Non-inferiority margin.
#' \item \code{puissance}: Desired statistical power.
#' \item \code{alpha}: Type I error rate.
#' \item \code{kappa}: Allocation ratio \code{n1 / n2}.
#' \item \code{missing_prop}: Proportion of missing data.
#' \item \code{n1}, \code{n2}: Required sample sizes in groups 1 and 2.
#' \item \code{n_total}: Total required sample size.
#' \item \code{n1_pdv}, \code{n2_pdv}: Required sample sizes after adjustment for missing data.
#' \item \code{n_total_pdv}: Total required sample size after adjustment for missing data.
#' }
#' Parameter combinations for which the non-inferiority calculation is not
#' feasible (\code{|p1 - p2| >= marge}) are silently removed from the result.
#' The result is assigned the attribute \code{ssdesignr_type = "prop_ni"},
#' which allows its validation when passed to \code{\link{ss_cluster}}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom purrr pmap map_dbl
#' @importFrom rpact getSampleSizeRates
#'
#' @examples
#' \dontrun{
#' # Chi-squared test with equal group sizes
# ss_prop_ni(
#   p1     = c(0.20, 0.30),
#   p2     = 0.70,
#   marge  = c(0.01, 0.15, 0.30),
#   power  = c(0.80, 0.90),
#   alpha  = 0.025
# )
#'
#' # Chi-squared test with unequal group sizes (kappa = 2)
# ss_prop_ni(
#   p1     = 0.20,
#   p2     = 0.70,
#   marge  = c(0.01, 0.15, 0.30),
#   kappa  = 2,
#   missing_prop = c(0.05, 0.10)
# )
#' }
#'
#' @export

ss_prop_ni <- function(
    p1           = NULL,
    p2           = NULL,
    marge        = NULL,
    power        = 0.80,
    alpha        = 0.025,
    kappa        = 1,
    missing_prop = 0,
    sided        = 1,
    choice       = "khi2"
) {

  choice <- match.arg(choice, choices = "khi2")

  # ------------------------------------
  # Verifications generales
  # ------------------------------------
  if (is.null(p1) | is.null(p2))
    stop("Les proportions 'p1' et 'p2' doivent etre fournies.")

  if (any(p1 <= 0) | any(p1 >= 1) | any(p2 <= 0) | any(p2 >= 1))
    stop("Les proportions 'p1' et 'p2' doivent etre strictement comprises entre 0 et 1.")

  if (is.null(marge) | any(marge <= 0) | any(marge >= 1))
    stop("La marge de non-inferiorite 'marge' doit etre fournie et strictement comprise entre 0 et 1.")

  if (any(power <= 0) | any(power >= 1))
    stop("'power' doit etre compris entre 0 et 1.")

  if (any(alpha <= 0) | any(alpha >= 1))
    stop("'alpha' doit etre compris entre 0 et 1.")

  if (any(alpha > 0.05))
    warning("Une valeur de 'alpha' > 0.05 est inhabituelle pour un essai de non-inferiorite. Le choix usuel est un test unilateral avec alpha = 0.025.")

  if (length(kappa) != 1)
    stop("'kappa' ne peut contenir qu'une seule valeur.")

  if (kappa <= 0)
    stop("'kappa' doit etre strictement positif.")

  if (any(missing_prop < 0) | any(missing_prop >= 1))
    stop("'missing_prop' doit etre compris entre 0 (inclus) et 1 (exclus).")

  if (length(sided) != 1 || !sided %in% c(1, 2))
    stop("'sided' doit valoir 1 ou 2.")

  # ------------------------------------
  # Grille de parametres
  # ------------------------------------
    params <- expand.grid(
    p1           = p1,
    p2           = p2,
    marge        = marge,
    power        = power,
    alpha        = alpha,
    missing_prop = missing_prop
  )

  # ------------------------------------
  # Calculs
  # ------------------------------------
  res <- params |>
    dplyr::mutate(
      tmp = purrr::pmap(
        list(p1, p2, marge, power, alpha),
        function(p1, p2, marge, power, alpha) {

          # Khi-2 : kappa = 1 ou kappa != 1 -> rpact::getSampleSizeRates()

          if (choice == "khi2") {
            res_rpact <- getSampleSizeRates(
              thetaH0                = -marge,
              pi1                    = p1,
              pi2                    = p2,
              alpha                  = alpha,
              beta                   = 1 - power,
              sided                  = sided,
              allocationRatioPlanned = kappa
            )

            n <- ceiling(res_rpact$numberOfSubjects)
            n2 <- ceiling(n / (1 + kappa))
            n1 <- ceiling(kappa * n2)

            return(list(n1 = n1, n2 = n2))
          }
        }
      )
    ) |>
    dplyr::mutate(
      n1            = ceiling(purrr::map_dbl(tmp, ~.x$n1)),
      n2            = ceiling(purrr::map_dbl(tmp, ~.x$n2)),
      n_total       = n1 + n2,
      n1_pdv        = ceiling(n1 / (1 - missing_prop)),
      n2_pdv        = ceiling(n2 / (1 - missing_prop)),
      n_total_pdv    = n1_pdv + n2_pdv,
      missing_prop = missing_prop,
      puissance     = power,
      test          = choice,
      kappa         = kappa
    )

  # ------------------------------------
  # Labels
  # ------------------------------------
  cols <- c("test", "p1", "p2", "marge", "puissance", "alpha", "kappa", "missing_prop",
              "n1", "n2", "n_total", "n1_pdv", "n2_pdv", "n_total_pdv")
    res <- dplyr::select(res, dplyr::all_of(cols))
    labels <- list(
      test = "Test",
      p1 = "Proportion 1",
      p2 = "Proportion 2",
      marge = "Marge NI",
      alpha = "Alpha",
      puissance = "Puissance",
      kappa = "Ratio N1/N2",
      n1 = "N1",
      n2 = "N2",
      n_total = "N total",
      missing_prop = "% d.m",
      n1_pdv = "N1",
      n2_pdv = "N2",
      n_total_pdv = "N total"
    )

    labels <- labels[names(labels) %in% names(res)]
    res <- labelled::set_variable_labels(res, !!!labels)

  # ------------------------------------
  # Enregistrement
  # ------------------------------------
  attr(res, "ssdesignr_type") <- "prop_ni"
  return(res)
}
