#' Calculate sample size for a superiority trial with a continuous endpoint
#'
#' @description
#' Calculates the required sample size to compare two treatment groups for a
#' continuous endpoint in the context of a superiority trial.
#'
#' @param mu1 Numeric vector. Expected mean in group 1 (treatment group).
#' @param mu2 Numeric vector. Expected mean in group 2 (control group).
#' Combinations for which \code{mu1 == mu2} are automatically excluded.
#' @param sd Numeric vector. Common standard deviation for the two groups.
#' Required when \code{choice = "student"}.
#' @param sd1 Numeric vector. Standard deviation in group 1. Required when
#' \code{choice = "welch"} or \code{choice = "wilcoxon"}.
#' @param sd2 Numeric vector. Standard deviation in group 2. Required when
#' \code{choice = "welch"} or \code{choice = "wilcoxon"}.
#' @param thetaH0 Numeric. Difference between group means under the null
#' hypothesis. Defaults to \code{0}. Must contain a single value.
#' @param power Numeric vector. Desired statistical power. Defaults to \code{0.80}.
#' @param alpha Numeric vector. Type I error rate. Defaults to \code{0.05}.
#' @param kappa Numeric. Allocation ratio \code{n1 / n2}. Defaults to \code{1},
#' corresponding to equal group sizes. Must contain a single value.
#' A value greater than \code{1} allocates more subjects to group 1.
#' @param missing_prop Numeric vector. Proportion of missing data. Defaults to \code{0}.
#' Used to calculate \code{n1_pdv}, \code{n2_pdv}, and \code{n_total_pdv}.
#' @param sided Integer. Number of sides of the test: \code{1} for a one-sided test or
#' \code{2} for a two-sided test. Defaults to \code{2}.
#' @param nsim Integer. Number of simulations used for the Wilcoxon test. Defaults to \code{10000}.
#' @param seed Integer. Random seed used for simulations performed for the Wilcoxon test.
#' Defaults to \code{NULL}.
#' @param choice Character string. Statistical test used for the sample size calculation:
#' \itemize{
#' \item \code{"student"}: Student's t-test assuming equal variances.
#' \item \code{"welch"}: Welch's t-test allowing unequal variances.
#' \item \code{"wilcoxon"}: Wilcoxon-Mann-Whitney non-parametric test.
#' This method does not support unequal allocation through \code{kappa}.
#' }
#'
#' @return A data frame with one row for each combination of input parameters and the following columns:
#' \itemize{
#' \item \code{test}: Statistical method used.
#' \item \code{mu1}, \code{mu2}: Expected group means.
#' \item \code{thetaH0}: Difference under the null hypothesis, when applicable.
#' \item \code{sd}: Common standard deviation for Student's t-test.
#' \item \code{sd1}, \code{sd2}: Group-specific standard deviations for Welch's or Wilcoxon tests.
#' \item \code{puissance}: Desired statistical power.
#' \item \code{alpha}: Type I error rate.
#' \item \code{kappa}: Allocation ratio \code{n1 / n2}.
#' \item \code{missing_prop}: Proportion of missing data.
#' \item \code{n1}, \code{n2}: Required sample sizes in groups 1 and 2.
#' \item \code{n_total}: Total required sample size.
#' \item \code{n1_pdv}, \code{n2_pdv}: Required sample sizes after adjustment for missing data.
#' \item \code{n_total_pdv}: Total required sample size after adjustment for missing data.
#' }
#' The result is assigned the attribute \code{ssdesignr_type = "mean_sup"},
#' which allows its validation when passed to \code{\link{ss_cluster}}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom purrr pmap map_dbl
#' @importFrom rpact getSampleSizeMeans
#' @importFrom WMWssp WMWssp_minimize
#'
#' @examples
#' \dontrun{
#' # Student's t-test with equal group sizes
# ss_mean_sup(
#   mu1    = c(55, 60),
#   mu2    = 50,
#   sd     = 10,
#   power  = c(0.80, 0.90),
#   choice = "student"
# )
#
# # Welch's t-test with unequal group sizes (kappa = 1/2)
# ss_mean_sup(
#   mu1    = c(55, 60),
#   mu2    = 50,
#   sd1    = 10,
#   sd2    = 15,
#   kappa  = 1/2,
#   choice = "welch"
# )
#' }
#'
#' @export

ss_mean_sup <- function(
    mu1          = NULL,
    mu2          = NULL,
    sd           = NULL,
    sd1          = NULL,
    sd2          = NULL,
    seed         = NULL,
    thetaH0      = 0,
    power        = 0.80,
    alpha        = 0.05,
    kappa        = 1,
    missing_prop = 0,
    nsim         = 10000,
    sided        = 2,
    choice       = c("student", "welch", "wilcoxon")
) {

  choice <- match.arg(choice)

  # ------------------------------------
  # Verifications generales
  # ------------------------------------
  if (is.null(mu1) | is.null(mu2))
    stop("Les moyennes 'mu1' et 'mu2' doivent etre fournies.")

  if (!is.null(sd) && (!is.numeric(sd) || any(sd <= 0)))
    stop("'sd' doit etre numerique et strictement positif.")

  if (!is.null(sd1) && (!is.numeric(sd1) || any(sd1 <= 0)))
    stop("'sd1' doit etre numerique et strictement positif.")

  if (!is.null(sd2) && (!is.numeric(sd2) || any(sd2 <= 0)))
    stop("'sd2' doit etre numerique et strictement positif.")

  if (length(thetaH0) != 1)
    stop("'thetaH0' ne peut contenir qu'une seule valeur.")

  if (any(power <= 0) | any(power >= 1))
    stop("'power' doit etre compris entre 0 et 1.")

  if (any(alpha <= 0) | any(alpha >= 1))
    stop("'alpha' doit etre compris entre 0 et 1.")

  if (length(kappa) != 1)
    stop("'kappa' ne peut contenir qu'une seule valeur.")

  if (kappa <= 0)
    stop("'kappa' doit etre strictement positif.")

  if (any(missing_prop < 0) | any(missing_prop >= 1))
    stop("'missing_prop' doit etre compris entre 0 (inclus) et 1 (exclus).")

  if (length(sided) != 1 || !sided %in% c(1, 2))
    stop("'sided' doit valoir 1 ou 2.")

  if (!is.null(seed) && ((!is.numeric(seed) || length(seed) != 1)))
    stop("'seed' doit etre un entier scalaire ou NULL.")

  # ------------------------------------
  # Verifications specifiques au test
  # ------------------------------------
  if (choice == "student") {
    if (is.null(sd)) {
      stop("'sd' doit etre fourni pour le test de Student.")
    }
  }

  if (choice %in% c("welch", "wilcoxon")) {
    if (is.null(sd1) | is.null(sd2)) {
      stop("'sd1' et 'sd2' doivent etre fournis pour les tests de Welch et Wilcoxon.")
    }
  }

  if (choice == "wilcoxon" && kappa != 1) {
    warning("'WMWssp_minimize' ne prend pas en charge 'kappa'. ",
            "Le calcul est effectue avec le ratio optimal determine par la simulation (ignorant kappa).")
  }

  # ------------------------------------
  # Grille de parametres
  # ------------------------------------
  params <- expand.grid(
    mu1          = mu1,
    mu2          = mu2,
    power        = power,
    alpha        = alpha,
    missing_prop = missing_prop
  ) |>
    dplyr::filter(mu1 != mu2)

  # ------------------------------------
  # Calcul
  # ------------------------------------
  res <- params |>
    dplyr::mutate(
      tmp = purrr::pmap(
        list(mu1, mu2, power, alpha),
        function(mu1, mu2, power, alpha) {

          # Student : kappa = 1 ou kappa != 1  -> rpact::getSampleSizeMeans()

          if (choice == "student") {
              res_rpact <- getSampleSizeMeans(
                alternative            = (mu1-mu2),
                stDev                  = sd,
                thetaH0                = thetaH0,
                sided                  = sided,
                alpha                  = alpha,
                beta                   = 1-power,
                allocationRatioPlanned = kappa)

              n <- ceiling(res_rpact$numberOfSubjects)
              n2 <- ceiling(n / (1 + kappa))
              n1 <- ceiling(kappa * n2)

              return(list(n1 = n1, n2 = n2))
              }

          # Welch : kappa = 1 ou kappa != 1 -> rpact::getSampleSizeMeans()

          if (choice == "welch") {
              res_rpact <- getSampleSizeMeans(
                alternative            = (mu1-mu2),
                stDev                  = c(sd1, sd2),
                thetaH0                = thetaH0,
                sided                  = sided,
                alpha                  = alpha,
                beta                   = 1-power,
                allocationRatioPlanned = kappa)

              n <- ceiling(res_rpact$numberOfSubjects)
              n2 <- ceiling(n / (1 + kappa))
              n1 <- ceiling(kappa * n2)

              return(list(n1 = n1, n2 = n2))
              }


          # Wilcoxon : WMWssp::WMWssp_minimize()
          # Retourne n1 et n2 depuis la simulation (kappa non supporte).

          if (choice == "wilcoxon") {
            if (!is.null(seed)) set.seed(seed)
            x <- stats::rnorm(nsim, mean = mu1, sd = sd1)
            y <- stats::rnorm(nsim, mean = mu2, sd = sd2)

            r <- WMWssp::WMWssp_minimize(
              x          = x,
              y          = y,
              alpha      = alpha,
              power      = power,
              simulation = TRUE,
              nsim       = nsim
            )

            n1 <- r$result["n1 rounded", "Results"]
            n2 <- r$result["n2 rounded", "Results"]
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
      n_total_pdv   = n1_pdv + n2_pdv,
      missing_prop = missing_prop,
      puissance     = power,
      test          = choice,
      kappa         = kappa
    )

  # ------------------------------------
  # Labels
  # ------------------------------------
  if (choice == "student") {
      res <- dplyr::mutate(res, sd = sd) |> dplyr::relocate(test, mu1, mu2, sd, .before = puissance)
      cols <- c("test", "mu1", "mu2", "sd", "puissance", "alpha", "kappa", "missing_prop",
                "n1", "n2", "n_total", "n1_pdv", "n2_pdv", "n_total_pdv")
    } else {
      res <- dplyr::mutate(res, sd1 = sd1, sd2 = sd2) |> dplyr::relocate(test, mu1, mu2, sd1, sd2, .before = puissance)
      cols <- c("test", "mu1", "mu2", "sd1", "sd2", "puissance", "alpha", "kappa", "missing_prop",
                "n1", "n2", "n_total", "n1_pdv", "n2_pdv", "n_total_pdv")
    }

    labels_common <- list(
      test = "Test",
      mu1 = "Moyenne 1",
      mu2 = "Moyenne 2",
      alpha = "Alpha",
      puissance = "Puissance",
      kappa = "Ratio N1/N2",
      n1 = "N1",
      n2 = "N2",
      n_total = "N",
      missing_prop = "% d.m",
      n1_pdv = "N1 - avec d.m",
      n2_pdv = "N2 - avec d.m",
      n_total_pdv = "N total - avec d.m"
    )

    labels_student <- c(labels_common, list(sd = "Ecart-type commun"))
    labels_welch <- c(labels_common, list(sd1 = "Ecart-type 1", sd2 = "Ecart-type 2"))
    labels <-
      if (choice == "student") {labels_student}
    else {labels_welch}

    res <- dplyr::select(res, dplyr::all_of(cols))
    labels <- labels[names(labels) %in% names(res)]
    res <- labelled::set_variable_labels(res, !!!labels)

    # ------------------------------------
    # Enregistrement
    # ------------------------------------
    attr(res, "ssdesignr_type") <- "mean_sup"
    return(res)

}
