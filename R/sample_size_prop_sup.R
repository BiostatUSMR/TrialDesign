#' FONCTION ss_prop_sup()
#'
#' @description
#' Calcule le nombre de sujets necessaire pour comparer deux proportions
#' dans le cadre d'un essai de superiorite.
#'
#' @param p1 Numerique. Proportion attendue dans le groupe 1 (traitement), entre 0 et 1. Peut etre un vecteur. Non utilise pour McNemar.
#' @param p2 Numerique. Proportion attendue dans le groupe 2 (controle), entre 0 et 1. Peut etre un vecteur. Non utilise pour McNemar.
#' @param power Numerique. Puissance souhaitee. Par defaut 0.80. Peut etre un vecteur.
#' @param alpha Numerique. Risque de 1ere espece. Par defaut 0.05. Peut etre un vecteur.
#' @param kappa Numerique. Ratio n1/n2. Par defaut 1 (groupes equilibres). Non applicable pour McNemar (test apparie).
#' @param p01 Numerique. Proportion de paires discordantes (groupe 2 positif, groupe 1 negatif). Requis pour McNemar uniquement.
#' @param p10 Numerique. Proportion de paires discordantes (groupe 1 positif, groupe 2 negatif). Requis pour McNemar uniquement.
#' @param missing_prop Numerique. Taux de donnees manquantes. Par defaut 0. Peut etre un vecteur.
#' @param sided Numerique. Test unilatéral (1) ou bilatéral (2). Par défaut 2. Peut prendre la valeur 1 ou 2 uniquement.
#' @param alternative Caractere. Direction du test : \code{"two.sided"}, \code{"less"} ou \code{"greater"}. Par defaut \code{"two.sided"}.
#' @param choice Caractere. Test statistique a utiliser :
#' \itemize{
#'   \item \code{"khi2"} : Khi-2 d'independance. Utilise \code{getSampleSizeRates()}.
#'   \item \code{"fisher"} : Test exact de Fisher. Utilise \code{exact2x2::ss2x2()} avec \code{approx = TRUE}.
#'   \item \code{"mcnemar"} : Test de McNemar, donnees appariees. Utilise \code{exact2x2::powerPaired2x2()} par recherche iterative.  Requiert \code{p01} et \code{p10}. kappa sans objet.
#' }
#'
#' @return Un data.frame avec une ligne par combinaison de parametres et les colonnes :
#' \itemize{
#'   \item \code{test} : Methode utilisee.
#'   \item \code{puissance} : Puissance.
#'   \item \code{p1}, \code{p2} : Proportions (absent pour McNemar).
#'   \item \code{alpha} : Risque de 1ere espece.
#'   \item \code{kappa} : Ratio d'allocation n1/n2 (absent pour McNemar).
#'   \item \code{missing_prop} : Proportion de donnees manquantes.
#'   \item \code{n1}, \code{n2} : Effectifs par groupe (absent pour McNemar).
#'   \item \code{n_total} : Effectif total.
#'   \item \code{n1_pdv}, \code{n2_pdv} : Effectifs par groupe avec prise en compte des donnees manquantes (absent pour McNemar).
#'   \item \code{n_total_pdv} : Effectif total avec prise en compte des donnees manquantes.
#' }
#' Un attribut \code{ssdesignr_type = "prop_sup"} est attache au resultat
#' pour permettre la validation dans \code{ss_cluster()}.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom purrr pmap map_dbl
#' @importFrom rpact getSampleSizeRates
#' @importFrom exact2x2 ss2x2 powerPaired2x2
#'
#' @examples
#' \dontrun{
#' # Khi-2 equilibre
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
#' # Fisher desequilibre
# ss_prop_sup(
#   p1           = 0.20,
#   p2           = 0.70,
#   power        = 0.80,
#   kappa        = 0.5,
#   choice       = "fisher",
#   sided        = 2
# )
#'
#' # McNemar
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
    alternative  = "two.sided",
    sided = 2,
    choice       = c("khi2", "fisher", "mcnemar")
) {

  choice <- match.arg(choice)

  # --- Verifications generales ---
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


  # --- Verifications specifiques au test ---

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

  # if (choice == "fisher" & kappa != 1) {
  #   warning("'exact2x2::ss2x2' ne supporte pas le desequilibre (kappa != 1). ",
  #           "Le calcul sera effectue pour des groupes equilibres (kappa = 1).")
  # }

  if (choice == "mcnemar") {
    if (!is.null(p1) | !is.null(p2)) {
      warning("Vous avez specifie p1 et/ou p2 pour le test de McNemar. ",
              "Ces parametres ne sont utilises que pour les tests non-apparies. ",
              "Pour McNemar, utilisez p01 et p10.")
    }
    if (is.null(p01) | is.null(p10)) {
      stop("Pour le test de McNemar, les proportions 'p01' et 'p10' doivent etre fournies.")
    }
    if (any(p01 + p10 > 1)) {
      stop("La somme p01 + p10 ne peut pas depasser 1.")
    }
  }

  # ===========================================================================
  # McNemar (traitement separe : pas de grille p1/p2, pas de n1/n2)
  # ===========================================================================

  if (choice == "mcnemar") {

    params <- expand.grid(
      power         = power,
      alpha         = alpha,
      missing_prop = missing_prop
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
        cat("Power:", current_power, "Alpha:", current_alpha, "N:", N, "\n")
        if (pw >= current_power) break
        N <- N + 1
      }
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

    labels <- list(
      test = "Test",
      puissance = "Puissance",
      alpha = "Alpha",
      missing_prop = "Proportion de données manquantes attendue",
      n_total = "Nombre de paires",
      n_total_pdv = "Nombre de paires - PDV pris en compte"
    )

    res <- labelled::set_variable_labels(res, !!!labels)

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
    missing_prop = missing_prop,
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(p1 != p2 | is.na(p1 != p2))

  res <- params |>
    dplyr::mutate(
      tmp = purrr::pmap(
        list(p1, p2, power, alpha),
        function(p1, p2, power, alpha) {

          # Khi-2 : kappa = 1 ou kappa != 1 -> rpact::getSampleSizeRates()

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

        # Fisher : kappa = 1 ou kappa != 1 -> exact2x2::ss2x2
        if (choice == "fisher") {

            # approx = TRUE pour eviter les temps de calcul excessifs

            res_exact2x2 <- exact2x2::ss2x2(
              p1          = p1,
              p0          = p2,
              power       = power,
              sig.level   = alpha,
              alternative = alternative,
              n1.over.n0  = kappa,
              paired      = FALSE,
              approx      = TRUE
            )

            n2 <- ceiling(res_exact2x2$n0)
            n1 <- ceiling(kappa * n2)
            # Pour garantir le ratio exact :
            n2 <- ceiling(n1/kappa)
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

  # Création des labels et sélection des variables à retourner
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
    missing_prop = "Proportion de données manquantes attendue",
    n1_pdv = "N1 - PDV pris en compte",
    n2_pdv = "N2 - PDV pris en compte",
    n_total_pdv = "N total - PDV"
  )

  labels <- labels[names(labels) %in% names(res)]
  res <- labelled::set_variable_labels(res, !!!labels)

  attr(res, "ssdesignr_type") <- "prop_sup"
  return(res)
}



