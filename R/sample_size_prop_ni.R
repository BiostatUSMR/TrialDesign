#' FONCTION ss_prop_ni()
#'
#' @description
#' Calcule la taille d'echantillon necessaire pour demontrer la non-inferiorite
#' entre deux groupes sur un critere binaire (deux proportions).
#'
#' Hypotheses :
#' \itemize{
#'   \item H0 : p1 - p2 <= -marge (inferiorite)
#'   \item H1 : p1 - p2 >  -marge (non-inferiorite)
#' }
#'
#' @note Le test exact de Fisher n'est pas disponible en NI dans pwrss 1.0.0
#' (\code{power.exact.twoprops} ne supporte pas le parametre \code{margin}).
#' Seul le khi-2 est disponible via \code{pwrss::power.z.twoprops()}.
#'
#' @param p1 Numerique. Proportion attendue dans le groupe 1 (traitement), entre 0 et 1.
#'   Peut etre un vecteur pour explorer plusieurs scenarios.
#' @param p2 Numerique. Proportion attendue dans le groupe 2 (controle), entre 0 et 1.
#'   Peut etre un vecteur.
#' @param marge Numerique. Marge de non-inferiorite (valeur strictement positive).
#'   Peut etre un vecteur.
#' @param power Numerique. Puissance souhaitee. Par defaut 0.80. Peut etre un vecteur.
#' @param alpha Numerique. Niveau de significativite unilateral. Par defaut 0.025.
#'   Peut etre un vecteur.
#' @param kappa Numerique. Ratio n1/n2. Par defaut 1 (groupes equilibres).
#' @param missing_rate Numerique. Taux de donnees manquantes. Par defaut 0.
#'   Peut etre un vecteur.
#' @param choice Caractere. Unique valeur acceptee : \code{"khi2"}.
#'   Conserve pour uniformite avec les autres fonctions du package.
#'
#' @return Un data.frame avec une ligne par combinaison de parametres et les colonnes :
#' \itemize{
#'   \item \code{test} : Methode utilisee (\code{"khi2"}).
#'   \item \code{puissance} : Puissance cible.
#'   \item \code{p1}, \code{p2} : Proportions.
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
#' Un attribut \code{ssdesignr_type = "prop_ni"} est attache au resultat
#' pour permettre la validation dans \code{ss_cluster()}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom purrr pmap map_dbl
#' @importFrom pwrss power.z.twoprops
#'
#' @examples
#' \dontrun{
#' # Khi-2 equilibre — cas de reference
#' ss_prop_ni(
#'   p1     = 0.75,
#'   p2     = 0.50,
#'   marge  = c(0.5, 0.1),
#'   power  = c(0.80, 0.90)
#' )
#'
#' # Khi-2 desequilibre (kappa = 2)
#' ss_prop_ni(
#'   p1     = c(0.75, 0.60),
#'   p2     = c(0.50, 0.55),
#'   marge  = c(0.1, 0.2),
#'   kappa  = 2
#' )
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
    missing_rate = 0,
    choice       = "khi2"
) {

  choice <- match.arg(choice, choices = "khi2")

  # --- Verifications generales ---

  if (is.null(p1) | is.null(p2)) {
    stop("Les proportions 'p1' et 'p2' doivent etre fournies.")
  }

  if (any(p1 <= 0) | any(p1 >= 1) | any(p2 <= 0) | any(p2 >= 1)) {
    stop("Les proportions 'p1' et 'p2' doivent etre strictement comprises entre 0 et 1.")
  }

  if (is.null(marge) | any(marge <= 0)) {
    stop("La marge de non-inferiorite 'marge' doit etre fournie et strictement positive.")
  }

  if (any(alpha <= 0) | any(alpha >= 1)) {
    stop("'alpha' doit etre compris entre 0 et 1.")
  }

  if (any(alpha > 0.5)) {
    warning("En non-inferiorite le test est unilateral : 'alpha' est conventionnellement <= 0.5 (typiquement 0.025).")
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

  # --- Utilitaire interne ---

  arrondir <- function(x) ceiling(as.numeric(x))

  # --- Grille de combinaisons de parametres ---

  params <- expand.grid(
    p1           = p1,
    p2           = p2,
    marge        = marge,
    power        = power,
    alpha        = alpha,
    missing_prop = missing_rate
  )

  # --- Calculs par ligne ---

  res <- params %>%
    dplyr::mutate(
      tmp = purrr::pmap(
        list(p1, p2, marge, power, alpha),
        function(p1, p2, marge, power, alpha) {

          # Khi-2 d'independance (equilibre ou desequilibre via kappa)
          # pwrss::power.z.twoprops() avec margin = -marge, alternative = "one.sided"
          # n.ratio = kappa (= n1/n2, convention pwrss)
          # Renvoie r$n = c(n1, n2).

          tryCatch({
            r <- pwrss::power.z.twoprops(
              prob1       = p1,
              prob2       = p2,
              margin      = -marge,
              power       = power,
              alpha       = alpha,
              alternative = "one.sided",
              n.ratio     = kappa,
              verbose     = FALSE
            )
            if (is.null(r$n) || any(is.na(r$n))) return(list(n1 = NA_real_, n2 = NA_real_))
            list(n1 = r$n[1], n2 = r$n[2])
          }, error = function(e) {
            list(n1 = NA_real_, n2 = NA_real_)
          })

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
    dplyr::filter(!is.na(n1)) %>%
    dplyr::select(
      test, puissance, p1, p2, marge, alpha, kappa,
      prop_manquant, n1, n2, n_total, n1_pdv, n2_pdv, ntotal_pdv
    )

  attr(res, "ssdesignr_type") <- "prop_ni"

  return(res)
}
