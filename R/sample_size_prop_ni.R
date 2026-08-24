#' FONCTION ss_prop_ni()
#'
#' @description
#' Calcule le nombre de sujets necessaire pour demontrer la non-inferiorite
#' entre deux groupes sur un critere binaire (deux proportions).
#'
#' Hypotheses :
#' \itemize{
#'   \item H0 : p1 - p2 <= -marge (inferiorite)
#'   \item H1 : p1 - p2 >  -marge (non-inferiorite)
#' }
#'
#' @note Seul le test du Khi-2 est disponible en NI.
#'
#' @param p1 Numerique. Proportion attendue dans le groupe 1 (traitement), entre 0 et 1. Peut etre un vecteur.
#' @param p2 Numerique. Proportion attendue dans le groupe 2 (controle), entre 0 et 1. Peut etre un vecteur.
#' @param marge Numerique. Marge de non-inferiorite (valeur strictement positive). Peut etre un vecteur.
#' @param power Numerique. Puissance souhaitee. Par defaut 0.80. Peut etre un vecteur.
#' @param alpha Numerique. Risque de 1ere espece. Par defaut 0.025. Peut etre un vecteur.
#' @param kappa Numerique. Ratio n1/n2. Par defaut 1 (groupes equilibres).
#' @param missing_prop Numerique. Taux de donnees manquantes. Par defaut 0. Peut etre un vecteur.
#' @param sided Numerique. Test unilateral (1) ou bilateral (2). Par defaut 1. Peut prendre la valeur 1 ou 2 uniquement.
#' @param choice Caractere. Unique valeur acceptee : \code{"khi2"}. Conserve pour uniformite avec les autres fonctions du package.
#'
#' @return Un data.frame avec une ligne par combinaison de parametres et les colonnes :
#' \itemize{
#'   \item \code{test} : Methode utilisee (\code{"khi2"}).
#'   \item \code{puissance} : Puissance.
#'   \item \code{p1}, \code{p2} : Proportions.
#'   \item \code{marge} : Marge de non-inferiorite.
#'   \item \code{alpha} : Risque de 1ere espece.
#'   \item \code{kappa} : Ratio d'allocation n1/n2.
#'   \item \code{missing_prop} : Proportion de donnees manquantes.
#'   \item \code{n1}, \code{n2} : Effectifs par groupe.
#'   \item \code{n_total} : Effectif total.
#'   \item \code{n1_pdv}, \code{n2_pdv} : Effectifs par groupe avec prise en compte des donnees manquantes.
#'   \item \code{n_total_pdv} : Effectif total avec prise en compte des donnees manquantes.
#' }
#' Les combinaisons infaisables (|delta| >= marge) sont silencieusement retirees.
#' Un attribut \code{ssdesignr_type = "prop_ni"} est attache au resultat
#' pour permettre la validation dans \code{ss_cluster()}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom purrr pmap map_dbl
#' @importFrom rpact getSampleSizeRates
#'
#' @examples
#' \dontrun{
#' # Khi-2 equilibre
# ss_prop_ni(
#   p1     = c(0.20, 0.30),
#   p2     = 0.70,
#   marge  = c(0.01, 0.15, 0.30),
#   power  = c(0.80, 0.90),
#   alpha  = 0.025
# )
#'
#' # Khi-2 desequilibre (kappa = 2)
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
  attr(res, "ssdesignr_type") <- "prop_ni"
  return(res)
}
