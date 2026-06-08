#' FONCTION ss_mean_ni()
#'
#' @description
#' Calcule la taille d'echantillon necessaire pour demontrer la non-inferiorite
#' entre deux groupes sur un critere continu (deux moyennes).
#'
#' Hypotheses :
#' \itemize{
#'   \item H0 : mu1 - mu2 <= -marge (inferiorite)
#'   \item H1 : mu1 - mu2 >  -marge (non-inferiorite)
#' }
#'
#' @param mu1 Numerique. Moyenne attendue dans le groupe 1 (traitement).
#'   Peut etre un vecteur pour explorer plusieurs scenarios.
#' @param mu2 Numerique. Moyenne attendue dans le groupe 2 (controle).
#'   Peut etre un vecteur pour explorer plusieurs scenarios.
#' @param sd Numerique. Ecart-type commun aux deux groupes.
#'   Requis pour le test de Student.
#' @param sd1 Numerique. Ecart-type du groupe 1.
#'   Requis pour les tests de Welch et Wilcoxon.
#' @param sd2 Numerique. Ecart-type du groupe 2.
#'   Requis pour les tests de Welch et Wilcoxon.
#' @param marge Numerique. Marge de non-inferiorite (valeur strictement positive).
#'   Peut etre un vecteur pour explorer plusieurs marges.
#' @param power Numerique. Puissance souhaitee. Par defaut 0.80.
#'   Peut etre un vecteur pour explorer plusieurs niveaux de puissance.
#' @param alpha Numerique. Niveau de significativite unilateral. Par defaut 0.025.
#'   Peut etre un vecteur.
#' @param kappa Numerique. Ratio n1/n2. Par defaut 1 (groupes equilibres).
#'   Un ratio > 1 indique plus de sujets dans le groupe 1.
#' @param missing_rate Numerique. Taux de donnees manquantes. Par defaut 0.
#'   Peut etre un vecteur. Utilise pour calculer n1_pdv, n2_pdv et ntotal_pdv.
#' @param choice Caractere. Test statistique a utiliser :
#' \itemize{
#'   \item \code{"student"} : Test de Student, variances egales.
#'     Utilise \code{pwrss::power.t.student()}.
#'   \item \code{"welch"} : Test de Welch, variances inegales.
#'     Utilise \code{pwrss::power.t.welch()}.
#'   \item \code{"wilcoxon"} : Test de Wilcoxon-MWW, non parametrique.
#'     Utilise \code{pwrss::power.np.wilcoxon()}.
#' }
#'
#' @return Un data.frame avec une ligne par combinaison de parametres et les colonnes :
#' \itemize{
#'   \item \code{test} : Methode utilisee.
#'   \item \code{puissance} : Puissance cible.
#'   \item \code{mu1}, \code{mu2} : Moyennes.
#'   \item \code{marge} : Marge de non-inferiorite.
#'   \item \code{alpha} : Niveau de significativite.
#'   \item \code{kappa} : Ratio d'allocation n1/n2.
#'   \item \code{prop_manquant} : Taux de manquants.
#'   \item \code{n1}, \code{n2} : Effectifs par groupe.
#'   \item \code{n_total} : Effectif total brut.
#'   \item \code{n1_pdv}, \code{n2_pdv} : Effectifs par groupe ajustes aux donnees manquantes.
#'   \item \code{ntotal_pdv} : Effectif total ajuste aux donnees manquantes.
#' }
#' Les combinaisons infaisables (|delta| >= marge) sont silencieusement retirees.
#' Un attribut \code{ssdesignr_type = "mean_ni"} est attache au resultat
#' pour permettre la validation dans \code{ss_cluster()}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom magrittr %>%
#' @importFrom purrr pmap map_dbl
#' @importFrom pwrss power.t.student power.t.welch power.np.wilcoxon
#'
#' @examples
#' \dontrun{
#' # Student equilibre — cas de reference
#' ss_mean_ni(
#'   mu1    = 10,
#'   mu2    = 10,
#'   sd     = 3,
#'   marge  = c(0.5, 1.5),
#'   power  = c(0.80, 0.90),
#'   choice = "student"
#' )
#'
#' # Welch desequilibre (kappa = 2)
#' ss_mean_ni(
#'   mu1    = 10,
#'   mu2    = 10,
#'   sd1    = 3,
#'   sd2    = 5,
#'   marge  = 0.5,
#'   kappa  = 2,
#'   choice = "welch"
#' )
#' }
#'
#' @export

ss_mean_ni <- function(
    mu1          = NULL,
    mu2          = NULL,
    sd           = NULL,
    sd1          = NULL,
    sd2          = NULL,
    marge        = NULL,
    power        = 0.80,
    alpha        = 0.025,
    kappa        = 1,
    missing_rate = 0,
    choice       = c("student", "welch", "wilcoxon")
) {

  choice <- match.arg(choice)

  # --- Verifications generales ---

  if (is.null(mu1) | is.null(mu2)) {
    stop("Les moyennes 'mu1' et 'mu2' doivent etre fournies.")
  }

  if (is.null(marge) | any(marge <= 0)) {
    stop("La marge de non-inferiorite 'marge' doit etre fournie et strictement positive.")
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

  # --- Utilitaire interne ---

  arrondir <- function(x) ceiling(as.numeric(x))

  # --- Grille de combinaisons de parametres ---

  params <- expand.grid(
    mu1          = mu1,
    mu2          = mu2,
    marge        = marge,
    power        = power,
    alpha        = alpha,
    missing_prop = missing_rate
  )

  # --- Calculs par ligne ---

  res <- params %>%
    dplyr::mutate(
      tmp = purrr::pmap(
        list(mu1, mu2, marge, power, alpha),
        function(mu1, mu2, marge, power, alpha) {

          delta <- mu1 - mu2

          if (choice == "student") {
            tryCatch({
              r <- pwrss::power.t.student(
                d           = delta / sd,
                margin      = -marge / sd,
                power       = power,
                alpha       = alpha,
                alternative = "one.sided",
                design      = "independent",
                n.ratio     = kappa,
                verbose     = FALSE
              )
              list(n1 = r$n[1], n2 = r$n[2])
            }, error = function(e) {
              list(n1 = NA_real_, n2 = NA_real_)
            })

          } else if (choice == "welch") {
            sd_pool <- sqrt((sd1^2 + sd2^2) / 2)
            tryCatch({
              r <- pwrss::power.t.welch(
                d           = delta / sd_pool,
                margin      = -marge / sd_pool,
                var.ratio   = sd1^2 / sd2^2,
                n.ratio     = kappa,
                power       = power,
                alpha       = alpha,
                alternative = "one.sided",
                verbose     = FALSE
              )
              list(n1 = r$n[1], n2 = r$n[2])
            }, error = function(e) {
              list(n1 = NA_real_, n2 = NA_real_)
            })

          } else if (choice == "wilcoxon") {
            sd_pool <- sqrt((sd1^2 + sd2^2) / 2)
            tryCatch({
              r <- pwrss::power.np.wilcoxon(
                d           = delta / sd_pool,
                margin      = -marge / sd_pool,
                power       = power,
                alpha       = alpha,
                alternative = "one.sided",
                design      = "independent",
                n.ratio     = kappa,
                verbose     = FALSE
              )
              list(n1 = r$n[1], n2 = r$n[2])
            }, error = function(e) {
              list(n1 = NA_real_, n2 = NA_real_)
            })

          } else {
            list(n1 = NA_real_, n2 = NA_real_)
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
      test, puissance, mu1, mu2, marge, alpha, kappa,
      prop_manquant, n1, n2, n_total, n1_pdv, n2_pdv, ntotal_pdv
    )

  res <- dplyr::filter(res, !is.na(n1))

  attr(res, "ssdesignr_type") <- "mean_ni"

  return(res)
}
