#' Calculate sample size for a superiority trial with a binary endpoint
#'
#' @description
#' Calculates the required sample size to compare two treatment groups for a
#' binary endpoint in the context of a superiority trial.
#'
#' @param p1 Numeric vector. Expected proportion in group 1 (treatment group),
#' ranging from 0 to 1. Not used when \code{choice = "mcnemar"}.
#' @param p2 Numeric vector. Expected proportion in group 2 (control group),
#' ranging from 0 to 1. Not used when \code{choice = "mcnemar"}.
#' @param power Numeric vector. Desired statistical power. Defaults to \code{0.80}.
#' @param alpha Numeric vector. Type I error rate. Defaults to \code{0.05}.
#' @param kappa Numeric. Allocation ratio \code{n1 / n2}. Defaults to
#' \code{1}, corresponding to equal group sizes. Not applicable when
#' \code{choice = "mcnemar"}, as the data are paired.
#' @param p01 Numeric vector. Proportion of discordant pairs for which group 2
#' is positive and group 1 is negative. Required only when \code{choice = "mcnemar"}.
#' @param p10 Numeric vector. Proportion of discordant pairs for which group 1
#' is positive and group 2 is negative. Required only when \code{choice = "mcnemar"}.
#' @param missing_prop Numeric vector. Proportion of missing data. Defaults to \code{0}.
#' @param sided Integer. Number of sides of the test: \code{1} for a one-sided
#' test or \code{2} for a two-sided test. Defaults to \code{2}. For a
#' one-sided test, the direction of the alternative hypothesis is
#' automatically determined from the sign of the expected difference
#' (\code{p1 - p2}, or \code{p10 - p01} for McNemar's test).
#' @param choice Character string. Statistical test used for the sample size calculation:
#' \itemize{
#' \item \code{"khi2"}: Chi-squared test of independence.
#' \item \code{"fisher"}: Fisher's exact test, using an approximation for the sample size calculation.
#' \item \code{"mcnemar"}: McNemar's test for paired binary data.
#'  Requires \code{p01} and \code{p10}. The allocation ratio
#' \code{kappa} is not applicable.
#' }
#'
#' @return A data frame with one row for each combination of input parameters. The
#' returned columns depend on the selected statistical method:
#' \itemize{
#' \item \code{test}: Statistical method used.
#' \item \code{p1}, \code{p2}: Expected proportions in groups 1 and 2. Not returned for McNemar's test.
#' \item \code{alpha}: Type I error rate.
#' \item \code{kappa}: Allocation ratio \code{n1 / n2}. Not returned for McNemar's test.
#' \item \code{puissance}: Desired statistical power.
#' \item \code{missing_prop}: Proportion of missing data.
#' \item \code{n1}, \code{n2}: Required sample sizes in groups 1 and 2. Not returned for McNemar's test.
#' \item \code{n_total}: Total required sample size.
#' \item \code{n1_pdv}, \code{n2_pdv}: Required sample sizes after adjustment for missing data. Not returned for McNemar's test.
#' \item \code{n_total_pdv}: Total required sample size after adjustment for missing data.
#' }
#' The result is assigned the attribute \code{ssdesignr_type = "prop_sup"},
#' which allows its validation when passed to \code{\link{ss_cluster}}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom purrr pmap map_dbl
#' @importFrom rpact getSampleSizeRates
#' @importFrom exact2x2 ss2x2 powerPaired2x2
#'
#' @examples
#' \dontrun{
#' # Chi-squared test with equal group sizes
# ss_prop_sup(
#   p1           = c(0.20, 0.30),
#   p2           = 0.70,
#   power        = c(0.80, 0.90),
#   missing_prop = 0.5,
#   kappa        = 1,
#   choice       = "khi2",
#   sided        = 2
# )
#
#' # Fisher's exact test with unequal group sizes
# ss_prop_sup(
#   p1           = 0.20,
#   p2           = 0.70,
#   power        = 0.80,
#   kappa        = 0.5,
#   choice       = "fisher",
#   sided        = 2
# )
#'
#' # McNemar's test for paired data
# ss_prop_sup(
#   p01    = 0.10,
#   p10    = 0.20,
#   power  = c(0.80, 0.90),
#   choice = "mcnemar"
# )
#' }
#'
#' @export

ss_prop_sup <- function(
    p1           = NULL,
    p2           = NULL,
    power        = 0.80,
    alpha        = 0.05,
    kappa        = 1,
    p01          = NULL,
    p10          = NULL,
    missing_prop = 0,
    sided = 2,
    choice       = c("khi2", "fisher", "mcnemar")
) {

  choice <- match.arg(choice)

  # ------------------------------------
  # Verifications generales
  # ------------------------------------
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


  # ------------------------------------
  # Verifications specifiques au test
  # ------------------------------------
  if (choice %in% c("khi2", "fisher")) {
    if (!is.null(p01) | !is.null(p10)) {
      warning("Vous avez specifie p01 et/ou p10 pour un test ", choice, ". ",
              "Ces parametres ne sont utilises que pour le test de McNemar. ",
              "Pour un test non-apparie, utilisez p1 et p2.")
    }
    if (is.null(p1) | is.null(p2)) {
      stop("Pour le test ", choice, ", les proportions 'p1' et 'p2' doivent etre fournies.")
    }
    if (any(p1 <= 0) | any(p1 >= 1) | any(p2 <= 0) | any(p2 >= 1)) {
      stop("Les proportions 'p1' et 'p2' doivent etre strictement comprises entre 0 et 1.")
    }
  }

if (choice == "mcnemar") {
    if (!is.null(p1) | !is.null(p2)) {
      warning("Vous avez specifie p1 et/ou p2 pour le test de McNemar. ",
              "Ces parametres ne sont utilises que pour les tests non-apparies. ",
              "Pour McNemar, utilisez p01 et p10.")
    }
    if (is.null(p01) | is.null(p10))
      stop("Pour le test de McNemar, les proportions 'p01' et 'p10' doivent etre fournies.")

    if (any(p01 <= 0) | any(p10 <= 0))
      stop("'p01' et 'p10' doivent etre strictement positifs.")

    if (any(p01 + p10 > 1))
      stop("La somme p01 + p10 ne peut pas depasser 1.")
  }

  # ===========================================================================
  # McNemar (traitement separe : pas de grille p1/p2, pas de n1/n2)
  # ===========================================================================

    if (choice == "mcnemar") {

      alt <- if (sided == 2) {"two.sided"} else if (p10 > p01) {"greater"} else {"less"}

    # ------------------------------------
    # Grille de parametres
    # ------------------------------------
    params <- expand.grid(
      power         = power,
      alpha         = alpha,
      missing_prop = missing_prop
    )

    n_total_values <- numeric(nrow(params))

    # ------------------------------------
    # Calcul
    # ------------------------------------
    for (i in seq_len(nrow(params))) {
      current_power <- params$power[i]
      current_alpha <- params$alpha[i]

      N <- 5
      repeat {
        pw <- exact2x2::powerPaired2x2(
          pb          = p10,
          pc          = p01,
          npairs      = N,
          sig.level   = current_alpha,
          alternative = alt
        )$power
        cat("\r", paste0("Recherche de N (power = ", current_power, ", alpha = ", current_alpha,") : essai N = ", N, "   "), sep = "")

        if (pw >= current_power) break
        N <- N + 1
      }
      cat("\n")
      n_total_values[i] <- N
    }

    res <- params |>
      dplyr::mutate(
        n_total    = n_total_values,
        n_total_pdv = ifelse(
          ceiling(n_total / (1 - missing_prop)) %% 2 == 0,
          ceiling(n_total / (1 - missing_prop)),
          ceiling(n_total / (1 - missing_prop)) + 1
        ),
        puissance = power,
        test      = choice
      ) |>
      dplyr::select(test, puissance, alpha, missing_prop, n_total, n_total_pdv)

    # ------------------------------------
    # Labels
    # ------------------------------------
    labels <- list(
      test = "Test",
      puissance = "Puissance",
      alpha = "Alpha",
      missing_prop = "% d.m",
      n_total = "Nombre de paires",
      n_total_pdv = "Nombre de paires - PDV pris en compte"
    )

    res <- labelled::set_variable_labels(res, !!!labels)

    # ------------------------------------
    # Enregistrement
    # ------------------------------------
    attr(res, "ssdesignr_type") <- "prop_sup"
    return(res)
  }

  # ===========================================================================
  # Khi-2 et Fisher
  # ===========================================================================

  # ------------------------------------
  # Grille de parametres
  # ------------------------------------
  params <- expand.grid(
    p1           = p1,
    p2           = p2,
    power        = power,
    alpha        = alpha,
    missing_prop = missing_prop,
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(p1 != p2 | is.na(p1 != p2))

  # ------------------------------------
  # Calcul
  # ------------------------------------
  res <- params |>
    dplyr::mutate(
      tmp = purrr::pmap(
        list(p1, p2, power, alpha),
        function(p1, p2, power, alpha) {

          if (choice == "khi2") {
            res_rpact <- getSampleSizeRates(
              pi1                    = p1,
              pi2                    = p2,
              sided                  = sided,
              alpha                  = alpha,
              beta                   = 1 - power,
              allocationRatioPlanned = kappa
            )
            n <- ceiling(res_rpact$numberOfSubjects)
            n2 <- ceiling(n / (1 + kappa))
            n1 <- ceiling(kappa * n2)
            return(list(n1 = n1, n2 = n2))
          }

          if (choice == "fisher") {
            # ss2x2() n'accepte que "two.sided" / "one.sided" (pas "less"/"greater")
            alt_fisher <- if (sided == 2) "two.sided" else "one.sided"

            res_exact2x2 <- exact2x2::ss2x2(
              p1          = p1,
              p0          = p2,
              power       = power,
              sig.level   = alpha,
              alternative = alt_fisher,
              n1.over.n0  = kappa,
              paired      = FALSE,
              approx      = TRUE
            )
            n2 <- ceiling(res_exact2x2$n0)
            n1 <- ceiling(kappa * n2)
            n2 <- ceiling(n1 / kappa)
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
      missing_prop  = missing_prop,
      puissance     = power,
      test          = choice,
      kappa         = kappa
    )

  # ------------------------------------
  # Labels
  # ------------------------------------
  cols <- c("test", "p1", "p2", "puissance", "alpha", "kappa", "missing_prop",
              "n1", "n2", "n_total", "n1_pdv", "n2_pdv", "n_total_pdv")
  res <- dplyr::select(res, dplyr::all_of(cols))
  labels <- list(
    test = "Test",
    p1 = "Proportion 1",
    p2 = "Proportion 2",
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

  labels <- labels[names(labels) %in% names(res)]
  res <- labelled::set_variable_labels(res, !!!labels)

  # ------------------------------------
  # Enregistrement
  # ------------------------------------
  attr(res, "ssdesignr_type") <- "prop_sup"
  return(res)
}



