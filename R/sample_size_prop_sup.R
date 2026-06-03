#' FONCTION ss_prop_sup()
#'
#' @description
#' Calcule la taille d'echantillon necessaire pour comparer deux proportions
#' dans le cadre d'un essai de superiorite.
#'
#' @param p1 Numerique. Proportion attendue dans le groupe 1 (traitement), entre 0 et 1.
#'   Peut etre un vecteur. Non utilise pour McNemar.
#' @param p2 Numerique. Proportion attendue dans le groupe 2 (controle), entre 0 et 1.
#'   Peut etre un vecteur. Non utilise pour McNemar.
#' @param power Numerique. Puissance souhaitee. Par defaut 0.80. Peut etre un vecteur.
#' @param alpha Numerique. Niveau de significativite. Par defaut 0.05. Peut etre un vecteur.
#' @param kappa Numerique. Ratio n1/n2. Par defaut 1 (groupes equilibres).
#'   Non applicable pour Fisher (equilibre uniquement) et McNemar (test apparie).
#' @param p01 Numerique. Proportion de paires discordantes (groupe 2 positif, groupe 1 negatif).
#'   Requis pour McNemar uniquement.
#' @param p10 Numerique. Proportion de paires discordantes (groupe 1 positif, groupe 2 negatif).
#'   Requis pour McNemar uniquement.
#' @param missing_rate Numerique. Taux de donnees manquantes. Par defaut 0.
#'   Peut etre un vecteur.
#' @param alternative Caractere. Direction du test : \code{"two.sided"}, \code{"less"}
#'   ou \code{"greater"}. Par defaut \code{"two.sided"}.
#' @param choice Caractere. Test statistique a utiliser :
#' \itemize{
#'   \item \code{"khi2"} : Khi-2 d'independance.
#'     Si kappa = 1 : \code{stats::power.prop.test()}.
#'     Si kappa != 1 : \code{pwrss::power.z.twoprops()} avec n.ratio = kappa.
#'   \item \code{"fisher"} : Test exact de Fisher, groupes equilibres uniquement.
#'     Utilise \code{exact2x2::ss2x2()} avec \code{approx = TRUE}.
#'   \item \code{"mcnemar"} : Test de McNemar, donnees appariees.
#'     Utilise \code{exact2x2::powerPaired2x2()} par recherche iterative.
#'     Requiert \code{p01} et \code{p10}. kappa sans objet.
#' }
#'
#' @return Un data.frame avec une ligne par combinaison de parametres et les colonnes :
#' \itemize{
#'   \item \code{test} : Methode utilisee.
#'   \item \code{puissance} : Puissance cible.
#'   \item \code{p1}, \code{p2} : Proportions (absent pour McNemar).
#'   \item \code{alpha} : Niveau de significativite.
#'   \item \code{kappa} : Ratio d'allocation n1/n2 (absent pour McNemar).
#'   \item \code{prop_manquant} : Taux de manquants.
#'   \item \code{n1}, \code{n2} : Effectifs par groupe (absent pour McNemar).
#'   \item \code{n_total} : Effectif total brut.
#'   \item \code{n1_pdv}, \code{n2_pdv} : Effectifs ajustes aux manquants (absent pour McNemar).
#'   \item \code{ntotal_pdv} : Effectif total ajuste aux donnees manquantes.
#' }
#' Un attribut \code{ssdesignr_type = "prop_sup"} est attache au resultat
#' pour permettre la validation dans \code{ss_cluster()}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom purrr pmap map_dbl
#' @importFrom stats power.prop.test
#' @importFrom pwrss power.z.twoprops
#' @importFrom exact2x2 ss2x2 powerPaired2x2
#'
#' @examples
#' \dontrun{
#' # Khi-2 equilibre
#' ss_prop_sup(
#'   p1     = c(0.25, 0.30),
#'   p2     = c(0.40, 0.50),
#'   power  = c(0.80, 0.90),
#'   choice = "khi2"
#' )
#'
#' # Fisher
#' ss_prop_sup(
#'   p1     = c(0.25, 0.30),
#'   p2     = c(0.40, 0.50),
#'   power  = 0.80,
#'   choice = "fisher"
#' )
#'
#' # McNemar
#' ss_prop_sup(
#'   p01    = 0.20,
#'   p10    = 0.40,
#'   power  = c(0.80, 0.90),
#'   choice = "mcnemar"
#' )
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
    missing_rate = 0,
    alternative  = "two.sided",
    choice       = c("khi2", "fisher", "mcnemar")
) {

  choice <- match.arg(choice)

  # --- Verifications generales ---

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

  if (choice %in% c("khi2", "fisher")) {
    if (!is.null(p01) | !is.null(p10)) {
      warning("Vous avez specifie p01 et/ou p10 pour un test ", choice, ". ",
              "Ces parametres ne sont utilises que pour le test de McNemar. ",
              "Pour un test non-apparie, utilisez p1 et p2.")
    }
    if (is.null(p1) | is.null(p2)) {
      stop("Pour le test ", choice, ", 'p1' et 'p2' doivent etre fournis.")
    }
    if (any(p1 <= 0) | any(p1 >= 1) | any(p2 <= 0) | any(p2 >= 1)) {
      stop("Les proportions 'p1' et 'p2' doivent etre strictement comprises entre 0 et 1.")
    }
  }

  if (choice == "fisher" & kappa != 1) {
    warning("'exact2x2::ss2x2' ne supporte pas le desequilibre (kappa != 1). ",
            "Le calcul sera effectue pour des groupes equilibres (kappa = 1).")
  }

  if (choice == "mcnemar") {
    if (!is.null(p1) | !is.null(p2)) {
      warning("Vous avez specifie p1 et/ou p2 pour le test de McNemar. ",
              "Ces parametres ne sont utilises que pour les tests non-apparies. ",
              "Pour McNemar, utilisez p01 et p10.")
    }
    if (is.null(p01) | is.null(p10)) {
      stop("Pour le test de McNemar, 'p01' et 'p10' doivent etre fournis.")
    }
    if (any(p01 + p10 > 1)) {
      stop("La somme p01 + p10 ne peut pas depasser 1.")
    }
  }

  # --- Utilitaire interne ---

  arrondir <- function(x) ceiling(as.numeric(x))

  # ===========================================================================
  # McNemar (traitement separe : pas de grille p1/p2, pas de n1/n2)
  # ===========================================================================

  if (choice == "mcnemar") {

    params <- expand.grid(
      power         = power,
      alpha         = alpha,
      prop_manquant = missing_rate,
      stringsAsFactors = FALSE
    )

    n_total_values <- numeric(nrow(params))

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
          alternative = alternative
        )$power
        if (pw >= current_power) break
        N <- N + 1
      }
      n_total_values[i] <- N
    }

    res <- params %>%
      dplyr::mutate(
        n_total    = n_total_values,
        ntotal_pdv = ifelse(
          ceiling(n_total / (1 - prop_manquant)) %% 2 == 0,
          ceiling(n_total / (1 - prop_manquant)),
          ceiling(n_total / (1 - prop_manquant)) + 1
        ),
        puissance = power,
        test      = choice
      ) %>%
      dplyr::select(test, puissance, alpha, prop_manquant, n_total, ntotal_pdv)

    attr(res, "ssdesignr_type") <- "prop_sup"

    return(res)
  }

  # ===========================================================================
  # Khi-2 et Fisher
  # ===========================================================================

  params <- expand.grid(
    p1           = p1,
    p2           = p2,
    power        = power,
    alpha        = alpha,
    missing_prop = missing_rate,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(p1 != p2)

  res <- params %>%
    dplyr::mutate(
      tmp = purrr::pmap(
        list(p1, p2, power, alpha),
        function(p1, p2, power, alpha) {

          # Khi-2 : kappa = 1 -> stats::power.prop.test()
          #         kappa != 1 -> pwrss::power.z.twoprops() avec n.ratio = kappa

          if (choice == "khi2") {
            if (kappa == 1) {
              n <- stats::power.prop.test(
                p1          = p1,
                p2          = p2,
                power       = power,
                sig.level   = alpha,
                alternative = alternative
              )$n
              return(list(n1 = n, n2 = n))
            } else {
              r <- pwrss::power.z.twoprops(
                prob1       = p1,
                prob2       = p2,
                n.ratio     = kappa,
                power       = power,
                alpha       = alpha,
                alternative = ifelse(alternative == "two.sided", "two.sided", "one.sided"),
                verbose     = FALSE
              )
              return(list(n1 = r$n[1], n2 = r$n[2]))
            }

          } else if (choice == "fisher") {

            # Fisher exact : exact2x2::ss2x2() — groupes equilibres uniquement
            # p0 = proportion groupe controle (p2)
            # approx = TRUE pour eviter les temps de calcul excessifs

            n <- exact2x2::ss2x2(
              p0          = p2,
              p1          = p1,
              power       = power,
              sig.level   = alpha,
              alternative = alternative,
              paired      = FALSE,
              approx      = TRUE
            )$n0
            return(list(n1 = n, n2 = n))

          } else {
            return(list(n1 = NA_real_, n2 = NA_real_))
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
    dplyr::filter(!is.na(n1)) %>%
    dplyr::select(
      test, puissance, p1, p2, kappa, alpha,
      prop_manquant, n1, n2, n_total, n1_pdv, n2_pdv, ntotal_pdv
    )

  attr(res, "ssdesignr_type") <- "prop_sup"

  return(res)
}
