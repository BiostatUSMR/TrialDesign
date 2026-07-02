#' FONCTION ss_mean_sup()
#'
#' @description
#' Calcule le nombre de sujets necessaire pour comparer deux moyennes
#' dans le cadre d'un essai de superiorite.
#'
#' @param mu1 Numerique. Moyenne attendue dans le groupe 1 (traitement). Peut etre un vecteur.
#' @param mu2 Numerique. Moyenne attendue dans le groupe 2 (controle). Peut etre un vecteur. Les combinaisons mu1 == mu2 sont automatiquement exclues.
#' @param sd Numerique. Ecart-type commun aux deux groupes. Requis pour le test de Student.
#' @param sd1 Numerique. Ecart-type du groupe 1. Requis pour les tests de Welch et Wilcoxon.
#' @param sd2 Numerique. Ecart-type du groupe 2. Requis pour les tests de Welch et Wilcoxon.
#' @param thetaH0 Numerique. Valeur sous l'hypothèse nulle. Par défaut 0. Ne peut pas etre un vecteur
#' @param power Numerique. Puissance souhaitee. Par defaut 0.80. Peut etre un vecteur.
#' @param alpha Numerique. Risque de premiere espece. Par defaut 0.05. Peut etre un vecteur.
#' @param kappa Numerique. Ratio n1/n2. Par defaut 1 (groupes equilibres). Ne peut pas etre un vecteur. Un ratio > 1 indique plus de sujets dans le groupe 1.
#' @param missing_prop Numerique. Taux de donnees manquantes. Par defaut 0. Peut etre un vecteur. Utilise pour calculer n1_pdv, n2_pdv et n_total_pdv.
#' @param sided Numerique. Test unilatéral (1) ou bilatéral (2). Par défaut 2. Peut prendre la valeur 1 ou 2 uniquement.
#' @param nsim Entier. Nombre de simulations pour le test de Wilcoxon. Par defaut 10000.
#' @param seed Entier. Graine utilisée pour les simulations du test de Wilcoxon. Par défaut NULL.
#' @param choice Caractere. Test statistique a utiliser :
#' \itemize{
#'   \item \code{"student"} : Test de Student, variances egales. Utilise \code{rpact::getSampleSizeMeans()}.
#'   \item \code{"welch"} : Test de Welch, variances inegales. Utilise \code{rpact::getSampleSizeMeans()}.
#'   \item \code{"wilcoxon"} : Test de Wilcoxon-MWW, non parametrique.
#'     Utilise \code{WMWssp::WMWssp_minimize()} (ne supporte pas kappa).
#' }
#'
#' @return Un data.frame avec une ligne par combinaison de parametres et les colonnes :
#' \itemize{
#'   \item \code{test} : Methode utilisee.
#'   \item \code{puissance} : Puissance.
#'   \item \code{mu1}, \code{mu2} : Moyennes.
#'   \item \code{alpha} : Risque de 1ere espece.
#'   \item \code{kappa} : Ratio d'allocation n1/n2.
#'   \item \code{missing_prop} : Proportion de donnees manquantes.
#'   \item \code{n1}, \code{n2} : Effectifs par groupe.
#'   \item \code{n_total} : Effectif total.
#'   \item \code{n1_pdv}, \code{n2_pdv} : Effectifs par groupe avec prise en compte des donnees manquantes.
#'   \item \code{n_total_pdv} : Effectif total avec prise en compte des donnees manquantes.
#' }
#' Un attribut \code{ssdesignr_type = "mean_sup"} est attache au resultat
#' pour permettre la validation dans \code{ss_cluster()}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom purrr pmap map_dbl
#' @importFrom rpact getSampleSizeMeans
#' @importFrom WMWssp WMWssp_minimize
#'
#' @examples
#' \dontrun{
#' # Student equilibre
# ss_mean_sup(
#   mu1    = c(55, 60),
#   mu2    = 50,
#   sd     = 10,
#   power  = c(0.80, 0.90),
#   choice = "student"
# )
#
# # Welch desequilibre (kappa = 1/2)
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

  # --- Verifications generales ---

  if (is.null(mu1) | is.null(mu2))
    stop("Les moyennes 'mu1' et 'mu2' doivent etre fournies.")

  if (!is.null(sd) && (!is.numeric(sd) || any(sd <= 0)))
    stop("'sd' doit être numérique et strictement positif.")

  if (!is.null(sd1) && (!is.numeric(sd1) || any(sd1 <= 0)))
    stop("'sd1' doit être numérique et strictement positif.")

  if (!is.null(sd2) && (!is.numeric(sd2) || any(sd2 <= 0)))
    stop("'sd2' doit être numérique et strictement positif.")

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
    stop("'seed' doit être un entier scalaire ou NULL.")

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

  if (choice == "wilcoxon" && kappa != 1) {
    warning("'WMWssp_minimize' ne prend pas en charge 'kappa'. ",
            "Le calcul est effectué avec le ratio optimal déterminé par la simulation (ignorant kappa).")
  }

  # --- Grille de combinaisons de parametres ---

  params <- expand.grid(
    mu1          = mu1,
    mu2          = mu2,
    power        = power,
    alpha        = alpha,
    missing_prop = missing_prop
  ) |>
    dplyr::filter(mu1 != mu2)

  # --- Calculs par ligne ---

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

    # Création des labels et sélection des variables à retourner
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
      missing_prop = "Proportion de données manquantes attendue",
      n1_pdv = "N1 - PDV pris en compte",
      n2_pdv = "N2 - PDV pris en compte",
      n_total_pdv = "N total - PDV"
    )

    labels_student <- c(labels_common, list(sd = "Ecart-type commun"))
    labels_welch <- c(labels_common, list(sd1 = "Ecart-type 1", sd2 = "Ecart-type 2"))
    labels <-
      if (choice == "student") {labels_student}
    else {labels_welch}

    res <- dplyr::select(res, dplyr::all_of(cols))
    labels <- labels[names(labels) %in% names(res)]
    res <- labelled::set_variable_labels(res, !!!labels)

    attr(res, "ssdesignr_type") <- "mean_sup"
    return(res)

}
