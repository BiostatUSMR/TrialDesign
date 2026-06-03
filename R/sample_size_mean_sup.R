#' FONCTION ss_mean_sup()
#'
#' @description
#' Calcule la taille d'echantillon necessaire pour comparer deux moyennes
#' dans le cadre d'un essai de superiorite.
#'
#' @param mu1 Numerique. Moyenne attendue dans le groupe 1 (traitement).
#'   Peut etre un vecteur pour explorer plusieurs scenarios.
#' @param mu2 Numerique. Moyenne attendue dans le groupe 2 (controle).
#'   Peut etre un vecteur. Les combinaisons mu1 == mu2 sont automatiquement exclues.
#' @param sd Numerique. Ecart-type commun aux deux groupes.
#'   Requis pour le test de Student.
#' @param sd1 Numerique. Ecart-type du groupe 1.
#'   Requis pour les tests de Welch et Wilcoxon.
#' @param sd2 Numerique. Ecart-type du groupe 2.
#'   Requis pour les tests de Welch et Wilcoxon.
#' @param power Numerique. Puissance souhaitee. Par defaut 0.80.
#'   Peut etre un vecteur.
#' @param alpha Numerique. Niveau de significativite. Par defaut 0.05.
#'   Peut etre un vecteur.
#' @param kappa Numerique. Ratio n1/n2. Par defaut 1 (groupes equilibres).
#' @param missing_rate Numerique. Taux de donnees manquantes. Par defaut 0.
#'   Peut etre un vecteur.
#' @param nsim Entier. Nombre de simulations pour le test de Wilcoxon. Par defaut 10000.
#' @param alternative Caractere. Direction du test : \code{"two.sided"} ou \code{"one.sided"}.
#'   Par defaut \code{"two.sided"}.
#' @param choice Caractere. Test statistique a utiliser :
#' \itemize{
#'   \item \code{"student"} : Test de Student, variances egales.
#'     Si kappa = 1 : \code{stats::power.t.test()}.
#'     Si kappa != 1 : \code{pwrss::power.t.student()}.
#'   \item \code{"welch"} : Test de Welch, variances inegales.
#'     Si kappa = 1 : \code{MKpower::power.welch.t.test()}.
#'     Si kappa != 1 : \code{pwrss::power.t.welch()}.
#'   \item \code{"wilcoxon"} : Test de Wilcoxon-MWW, non parametrique.
#'     Utilise \code{WMWssp::WMWssp_minimize()} (ne supporte pas kappa).
#' }
#'
#' @return Un data.frame avec une ligne par combinaison de parametres et les colonnes :
#' \itemize{
#'   \item \code{test} : Methode utilisee.
#'   \item \code{puissance} : Puissance cible.
#'   \item \code{mu1}, \code{mu2} : Moyennes.
#'   \item \code{alpha} : Niveau de significativite.
#'   \item \code{kappa} : Ratio d'allocation n1/n2.
#'   \item \code{prop_manquant} : Taux de manquants.
#'   \item \code{n1}, \code{n2} : Effectifs par groupe.
#'   \item \code{n_total} : Effectif total brut.
#'   \item \code{n1_pdv}, \code{n2_pdv} : Effectifs par groupe ajustes aux donnees manquantes.
#'   \item \code{ntotal_pdv} : Effectif total ajuste aux donnees manquantes.
#' }
#' Un attribut \code{ssdesignr_type = "mean_sup"} est attache au resultat
#' pour permettre la validation dans \code{ss_cluster()}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom purrr pmap map_dbl
#' @importFrom stats power.t.test
#' @importFrom MKpower power.welch.t.test
#' @importFrom pwrss power.t.student power.t.welch
#' @importFrom WMWssp WMWssp_minimize
#'
#' @examples
#' \dontrun{
#' # Student equilibre
#' ss_mean_sup(
#'   mu1    = c(55, 60),
#'   mu2    = 50,
#'   sd     = 10,
#'   power  = c(0.80, 0.90),
#'   choice = "student"
#' )
#'
#' # Welch desequilibre (kappa = 2)
#' ss_mean_sup(
#'   mu1    = c(55, 60),
#'   mu2    = 50,
#'   sd1    = 10,
#'   sd2    = 15,
#'   kappa  = 2,
#'   choice = "welch"
#' )
#' }
#'
#' @export

ss_mean_sup <- function(
    mu1          = NULL,
    mu2          = NULL,
    sd           = NULL,
    sd1          = NULL,
    sd2          = NULL,
    power        = 0.80,
    alpha        = 0.05,
    kappa        = 1,
    missing_rate = 0,
    nsim         = 10000,
    alternative  = "two.sided",
    choice       = c("student", "welch", "wilcoxon")
) {

  choice <- match.arg(choice)

  # --- Verifications generales ---

  if (is.null(mu1) | is.null(mu2)) {
    stop("Les moyennes 'mu1' et 'mu2' doivent etre fournies.")
  }

  if (any(alpha <= 0) | any(alpha >= 1)) {
    stop("'alpha' doit etre compris entre 0 et 1.")
  }

  if (any(power <= 0) | any(power >= 1)) {
    stop("'power' doit etre compris entre 0 et 1.")
  }

  if (any(missing_rate < 0) | any(missing_rate >= 1)) {
    stop("'missing_rate' doit etre compris entre 0 (inclus) et 1 (exclus).")
  }

  if (kappa <= 0) {
    stop("'kappa' doit etre strictement positif.")
  }

  # --- Verifications specifiques au test ---

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

  if (choice == "wilcoxon" & kappa != 1) {
    warning("'WMWssp_minimize' ne supporte pas de ratio d'allocation kappa. ",
            "Le resultat sera calcule avec le ratio optimal de la simulation (ignorant kappa).")
  }

  # --- Utilitaire interne ---

  arrondir  <- function(x) ceiling(as.numeric(x))
  alt_pwrss <- alternative

  # --- Grille de combinaisons de parametres ---

  params <- expand.grid(
    mu1          = mu1,
    mu2          = mu2,
    power        = power,
    alpha        = alpha,
    missing_prop = missing_rate
  ) %>%
    dplyr::filter(mu1 != mu2)

  # --- Calculs par ligne ---

  res <- params %>%
    dplyr::mutate(
      tmp = purrr::pmap(
        list(mu1, mu2, power, alpha),
        function(mu1, mu2, power, alpha) {

          # Student : kappa = 1 -> stats::power.t.test()
          #           kappa != 1 -> pwrss::power.t.student() avec n.ratio = kappa

          if (choice == "student") {
            if (kappa == 1) {
              n <- stats::power.t.test(
                n           = NULL,
                delta       = abs(mu1 - mu2),
                sd          = sd,
                sig.level   = alpha,
                power       = power,
                type        = "two.sample",
                alternative = alternative
              )$n
              return(list(n1 = n, n2 = n))
            } else {
              r <- pwrss::power.t.student(
                d           = abs(mu1 - mu2) / sd,
                n.ratio     = kappa,
                power       = power,
                alpha       = alpha,
                alternative = alt_pwrss,
                design      = "independent",
                verbose     = FALSE
              )
              return(list(n1 = r$n[1], n2 = r$n[2]))
            }
          }

          # Welch : kappa = 1 -> MKpower::power.welch.t.test()
          #         kappa != 1 -> pwrss::power.t.welch() avec n.ratio = kappa

          if (choice == "welch") {
            if (kappa == 1) {
              n <- MKpower::power.welch.t.test(
                n           = NULL,
                delta       = abs(mu1 - mu2),
                sd1         = sd1,
                sd2         = sd2,
                power       = power,
                sig.level   = alpha,
                alternative = alternative
              )$n
              return(list(n1 = n, n2 = n))
            } else {
              sd_pool <- sqrt((sd1^2 + sd2^2) / 2)
              r <- pwrss::power.t.welch(
                d           = abs(mu1 - mu2) / sd_pool,
                var.ratio   = sd1^2 / sd2^2,
                n.ratio     = kappa,
                power       = power,
                alpha       = alpha,
                alternative = alt_pwrss,
                verbose     = FALSE
              )
              return(list(n1 = r$n[1], n2 = r$n[2]))
            }
          }

          # Wilcoxon : WMWssp::WMWssp_minimize()
          # Retourne n1 et n2 depuis la simulation (kappa non supporte).

          if (choice == "wilcoxon") {
            set.seed(123)
            x <- rnorm(nsim, mean = mu1, sd = sd1)
            y <- rnorm(nsim, mean = mu2, sd = sd2)

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
    ) %>%
    dplyr::mutate(
      n1            = arrondir(purrr::map_dbl(tmp, ~.x$n1)),
      n2            = arrondir(purrr::map_dbl(tmp, ~.x$n2)),
      n_total       = n1 + n2,
      n1_pdv        = ceiling(n1 / (1 - missing_prop)),
      n2_pdv        = ceiling(n2 / (1 - missing_prop)),
      ntotal_pdv    = n1_pdv + n2_pdv,
      prop_manquant = missing_prop,
      puissance     = power,
      test          = choice,
      kappa         = kappa
    ) %>%
    dplyr::select(
      test, puissance, mu1, mu2, alpha, kappa,
      prop_manquant, n1, n2, n_total, n1_pdv, n2_pdv, ntotal_pdv
    )

  attr(res, "ssdesignr_type") <- "mean_sup"

  return(res)
}
