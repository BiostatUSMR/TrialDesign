#' FONCTION ss_mean_ni()
#'
#' @description
#' Calcule le nombre de sujets necessaire pour demontrer la non-inferiorite
#' entre deux groupes sur un critere continu (deux moyennes).
#'
#' Hypotheses :
#' \itemize{
#'   \item H0 : mu1 - mu2 <= -marge (inferiorite)
#'   \item H1 : mu1 - mu2 >  -marge (non-inferiorite)
#' }
#'
#' @param mu1 Numerique. Moyenne attendue dans le groupe 1 (traitement). Peut etre un vecteur.
#' @param mu2 Numerique. Moyenne attendue dans le groupe 2 (controle). Peut etre un vecteur.
#' @param sd Numerique. Ecart-type commun aux deux groupes. Requis pour le test de Student.
#' @param sd1 Numerique. Ecart-type du groupe 1. Requis pour le test de Welch.
#' @param sd2 Numerique. Ecart-type du groupe 2. Requis pour le test de Welch.
#' @param marge Numerique. Marge de non-inferiorite (valeur strictement positive). Peut etre un vecteur.
#' @param power Numerique. Puissance souhaitee. Par defaut 0.80. Peut etre un vecteur.
#' @param alpha Numerique. Risque de premiere espece. Par defaut 0.025. Peut etre un vecteur.
#' @param kappa Numerique. Ratio n1/n2. Par defaut 1 (groupes equilibres). Ne peut pas etre un vecteur. Un ratio > 1 indique plus de sujets dans le groupe 1.
#' @param missing_prop Numerique. Taux de donnees manquantes. Par defaut 0. Peut etre un vecteur. Utilise pour calculer n1_pdv, n2_pdv et n_total_pdv.
#' @param sided Numerique. Test unilateral (1) ou bilateral (2). Par defaut 1. Peut prendre la valeur 1 ou 2 uniquement.
#' @param choice Caractere. Test statistique a utiliser :
#' \itemize{
#'   \item \code{"student"} : Test de Student, variances egales. Utilise \code{rpact::getSampleSizeMeans()}.
#'   \item \code{"welch"} : Test de Welch, variances inegales. Utilise \code{rpact::getSampleSizeMeans()}.
#' }
#'
#' @return Un data.frame avec une ligne par combinaison de parametres et les colonnes :
#' \itemize{
#'   \item \code{test} : Methode utilisee.
#'   \item \code{puissance} : Puissance.
#'   \item \code{mu1}, \code{mu2} : Moyennes.
#'   \item \code{marge} : Marge de non-inferiorite.
#'   \item \code{alpha} : Risque de 1ere espece.
#'   \item \code{kappa} : Ratio d'allocation n1/n2.
#'   \item \code{missing_prop} : Proportion de donnees manquantes.
#'   \item \code{n1}, \code{n2} : Effectifs par groupe.
#'   \item \code{n_total} : Effectif total.
#'   \item \code{n1_pdv}, \code{n2_pdv} : Effectifs par groupe avec prise en compte des donnees manquantes.
#'   \item \code{n_total_pdv} : Effectif total avec prise en compte des donnees manquantes.
#' }
#' Un attribut \code{ssdesignr_type = "mean_ni"} est attache au resultat
#' pour permettre la validation dans \code{ss_cluster()}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom purrr pmap map_dbl
#' @importFrom rpact getSampleSizeMeans
#'
#' @examples
#' \dontrun{
#' # Student equilibre
# ss_mean_ni(
#   mu1    = 50,
#   mu2    = 50,
#   sd     = 10,
#   marge  = c(1, 5, 10),
#   power  = c(0.80, 0.90),
#   choice = "student"
# )
#'
#' # Welch desequilibre (kappa = 1/2)
# ss_mean_ni(
#   mu1    = 50,
#   mu2    = 50,
#   sd1    = 10,
#   sd2    = 15,
#   marge  = c(1, 5, 10),
#   kappa  = 1/2,
#   choice = "welch"
# )
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
    missing_prop = 0,
    sided        = 1,
    choice       = c("student", "welch")
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

  if (is.null(marge) | any(marge <= 0))
    stop("La marge de non-inferiorite 'marge' doit etre fournie et strictement positive.")

  if (any(power <= 0) | any(power >= 1))
    stop("'power' doit etre compris entre 0 et 1.")

  if (any(alpha <= 0) | any(alpha >= 1))
    stop("'alpha' doit etre compris entre 0 et 1.")

  if (any(alpha > 0.05))
    warning("Une valeur de 'alpha' > 0.05 est inhabituelle pour un essai de non-inferiorite. Le choix usuel est un test unilateral avec alpha = 0.025).")

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
  if (choice == "student") {
    if (is.null(sd)) {
      stop("'sd' doit etre fourni pour le test de Student.")
    }
  }
  if (choice == "welch") {
    if (is.null(sd1) | is.null(sd2)) {
      stop("'sd1' et 'sd2' doivent etre fournis pour le test de Welch.")
    }
  }

  # ------------------------------------
  # Grille de parametres
  # ------------------------------------
  params <- expand.grid(
    mu1          = mu1,
    mu2          = mu2,
    marge        = marge,
    power        = power,
    alpha        = alpha,
    missing_prop = missing_prop
  )

  # ------------------------------------
  # Calcul
  # ------------------------------------
  res <- params |>
    dplyr::mutate(
      tmp = purrr::pmap(
        list(mu1, mu2, marge, power, alpha),
        function(mu1, mu2, marge, power, alpha) {

          # Student : kappa = 1 ou kappa != 1  -> rpact::getSampleSizeMeans()

          if (choice == "student") {

            res_rpact <- getSampleSizeMeans(
              thetaH0                = -marge,
              alternative            = (mu1-mu2),
              alpha                  = alpha,
              beta                   = 1-power,
              stDev                  = sd,
              sided                  = sided,
              allocationRatioPlanned = kappa)

            n <- ceiling(res_rpact$numberOfSubjects)
            n2 <- ceiling(n / (1 + kappa))
            n1 <- ceiling(kappa * n2)

            return(list(n1 = n1, n2 = n2))
          }

          # Welch : kappa = 1 ou kappa != 1 -> rpact::getSampleSizeMeans()

          if (choice == "welch") {
            res_rpact <- getSampleSizeMeans(
              thetaH0                = -marge,
              alternative            = (mu1-mu2),
              alpha                  = alpha,
              beta                   = 1-power,
              stDev                  = c(sd1, sd2),
              sided                  = sided,
              allocationRatioPlanned = kappa)

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
  if (choice == "student") {
    res <- dplyr::mutate(res, sd = sd) |> dplyr::relocate(test, mu1, mu2, marge, sd, .before = puissance)
    cols <- c("test", "mu1", "mu2", "marge", "sd", "puissance", "alpha", "kappa", "missing_prop",
              "n1", "n2", "n_total", "n1_pdv", "n2_pdv", "n_total_pdv")
  } else {
    res <- dplyr::mutate(res, sd1 = sd1, sd2 = sd2) |> dplyr::relocate(test, mu1, mu2, marge, sd1, sd2, .before = puissance)
    cols <- c("test", "mu1", "mu2", "marge", "sd1", "sd2", "puissance", "alpha", "kappa", "missing_prop",
              "n1", "n2", "n_total", "n1_pdv", "n2_pdv", "n_total_pdv")
  }

  labels_common <- list(
    test = "Test",
    mu1 = "Moyenne 1",
    mu2 = "Moyenne 2",
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
  attr(res, "ssdesignr_type") <- "mean_ni"
  return(res)
}



