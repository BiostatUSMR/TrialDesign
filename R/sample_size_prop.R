#' Calcul de la taille d'\u00e9chantillon pour la comparaison de deux proportions
#'
#' @description
#' Calcule la taille d'\u00e9chantillon n\u00e9cessaire pour comparer deux proportions
#' selon diff\u00e9rents tests statistiques (Khi-2, Fisher, McNemar).
#' Supporte les groupes \u00e9quilibr\u00e9s et d\u00e9s\u00e9quilibr\u00e9s, et permet d'ajuster
#' l'effectif sur un taux de donn\u00e9es manquantes.
#'
#' @param p1 Vecteur num\u00e9rique. Proportion(s) attendue(s) dans le groupe 1,
#'   entre 0 et 1. Non utilis\u00e9 pour McNemar.
#' @param p2 Vecteur num\u00e9rique. Proportion(s) attendue(s) dans le groupe 2,
#'   entre 0 et 1. Non utilis\u00e9 pour McNemar.
#' @param power Vecteur num\u00e9rique. Puissance(s) souhait\u00e9e(s), entre 0 et 1.
#'   Par d\u00e9faut \code{0.80}.
#' @param alpha Vecteur num\u00e9rique. Niveau(x) de significativit\u00e9, entre 0 et 1.
#'   Par d\u00e9faut \code{0.05}.
#' @param kappa Num\u00e9rique. Ratio n2/n1 pour les designs d\u00e9s\u00e9quilibr\u00e9s.
#'   Par d\u00e9faut \code{1}.
#' @param p01 Num\u00e9rique. Pour McNemar uniquement. Proportion de paires
#'   discordantes (groupe 1 positif, groupe 2 n\u00e9gatif).
#' @param p10 Num\u00e9rique. Pour McNemar uniquement. Proportion de paires
#'   discordantes (groupe 1 n\u00e9gatif, groupe 2 positif).
#' @param missing_rate Vecteur num\u00e9rique. Taux de donn\u00e9es manquantes attendu,
#'   entre 0 et 1. Par d\u00e9faut \code{0}.
#' @param alternative Caract\u00e8re. Type de test : \code{"two.sided"} (par d\u00e9faut),
#'   \code{"less"} ou \code{"greater"}.
#' @param choice Caract\u00e8re. Test statistique \u00e0 utiliser. Valeurs possibles :
#'   \itemize{
#'     \item \code{"khi2_equilibre"} : test du Khi-2, groupes \u00e9quilibr\u00e9s
#'     \item \code{"khi2_desequilibre"} : test du Khi-2, groupes d\u00e9s\u00e9quilibr\u00e9s
#'     \item \code{"fisher"} : test exact de Fisher
#'     \item \code{"mcnemar"} : test de McNemar pour donn\u00e9es appari\u00e9es
#'   }
#'
#' @return Un data.frame contenant une ligne par combinaison de param\u00e8tres,
#'   avec les colonnes :
#'   \itemize{
#'     \item \code{test} : test utilis\u00e9
#'     \item \code{puissance} : puissance utilis\u00e9e
#'     \item \code{p1}, \code{p2} : proportions (sauf McNemar)
#'     \item \code{kappa} : ratio d'allocation (sauf McNemar)
#'     \item \code{alpha} : niveau de significativit\u00e9
#'     \item \code{prop_manquant} : taux de donn\u00e9es manquantes
#'     \item \code{n1}, \code{n2} : effectifs par groupe (sauf McNemar)
#'     \item \code{n_total} : effectif total brut
#'     \item \code{n_total_ajuste_manquant} : effectif total ajust\u00e9 sur le taux de manquants
#'   }
#'
#' @details
#' Pour le test de McNemar, \code{p01} et \code{p10} doivent \u00eatre fournis
#' \u00e0 la place de \code{p1} et \code{p2}, et leur somme ne doit pas d\u00e9passer 1.
#' Les effectifs sont syst\u00e9matiquement arrondis \u00e0 l'entier sup\u00e9rieur pair.
#'
#' @examples
#' \dontrun{
#' # Khi-2 \u00e9quilibr\u00e9
#' sample_size_prop(
#'   p1 = c(0.25, 0.30),
#'   p2 = c(0.40, 0.50),
#'   power = c(0.80, 0.90),
#'   missing_rate = 0.10,
#'   choice = "khi2_equilibre"
#' )
#'
#' # McNemar
#' sample_size_prop(
#'   power = 0.80,
#'   p01   = 0.20,
#'   p10   = 0.40,
#'   choice = "mcnemar"
#' )
#' }
#'
#' @importFrom dplyr mutate select filter
#' @importFrom purrr pmap map_dbl
#' @importFrom stats power.prop.test
#' @export

sample_size_prop <- function(
    p1 = NULL,
    p2 = NULL,
    power = 0.80,
    alpha = 0.05,
    kappa = 1,
    p01 = NULL,
    p10 = NULL,
    missing_rate = 0,
    alternative = "two.sided",
    choice = c("khi2_equilibre", "khi2_desequilibre", "fisher", "mcnemar")
) {

  choice <- match.arg(choice)
  arrondir <- function(x) ceiling(as.numeric(x))

  # Warnings

  if(choice %in% c("khi2_equilibre", "khi2_desequilibre", "fisher")) {
    if(!is.null(p01) || !is.null(p10)) {
      warning("Vous avez spécifié p01 et/ou p10 pour un test ", choice, ". ",
              "Ces paramètres ne sont utilisés que pour le test de McNemar. ",
              "Pour un test non-apparié, utilisez p1 et p2.")
    }

    if(is.null(p1) || is.null(p2)) {
      stop("Pour les tests ", choice, ", les arguments p1 et p2 doivent être spécifiés.")
    }
  }

  if(choice == "mcnemar") {
    if(!is.null(p1) || !is.null(p2)) {
      warning("Vous avez spécifié p1 et/ou p2 pour le test de McNemar. ",
              "Ces paramètres ne sont utilisés que pour les tests non-appariés. ",
              "Pour McNemar, utilisez p01 et p10.")
    }

    if(is.null(p01) || is.null(p10)) {
      stop("Pour le test de McNemar, les arguments p01 et p10 doivent être spécifiés.")
    }
    if(any(p01 + p10 > 1)) {
      stop("La somme p01 + p10 ne peut pas dépasser 1.")
    }
  }

  # McNemar
  if(choice == "mcnemar") {
    params <- expand.grid(
      power = power,
      alpha = alpha,
      prop_manquant = missing_rate,
      stringsAsFactors = FALSE
    )

    n_total_values <- numeric(nrow(params))
    for(i in 1:nrow(params)) {
      current_power <- params$power[i]
      current_alpha <- params$alpha[i]

      N <- 5
      repeat {
        pw <- exact2x2::powerPaired2x2(
          pb = p10,
          pc = p01,
          npairs = N,
          sig.level = current_alpha,
          alternative = alternative
        )$power
        if(pw >= current_power) break
        N <- N + 1
      }
      n_total_values[i] <- N
    }

    res <- params %>%
      dplyr::mutate(
        n_total = n_total_values,
        n_total_ajuste_manquant = ifelse(
          ceiling(n_total / (1 - prop_manquant)) %% 2 == 0,
          ceiling(n_total / (1 - prop_manquant)),
          ceiling(n_total / (1 - prop_manquant)) + 1
        ),
        puissance = power,
        test=choice
      ) %>%
      dplyr::select(test, puissance, alpha, prop_manquant, n_total, n_total_ajuste_manquant)

  } else {
    # CORRECTION : virgule enlevée et suppression de la ligne mutate redondante
    params <- expand.grid(
      power = power,
      p1 = p1,
      p2 = p2,
      alpha = alpha,
      missing_prop = missing_rate,  # <- virgule enlevée à la fin
      stringsAsFactors = FALSE
    ) %>%
      filter(is.na(p1) | is.na(p2) | p1 != p2)  # <- ligne mutate supprimée

    res <- params %>%
      dplyr::mutate(
        tmp = pmap(
          list(p1, p2, power, alpha),
          function(p1_val, p2_val, power_val, alpha_val) {
            # Khi 2 équilibré
            if(choice == "khi2_equilibre") {
              n <- stats::power.prop.test(
                p1 = p1_val,
                p2 = p2_val,
                power = power_val,
                sig.level = alpha_val
              )$n
              list(n1 = n, n2 = n)
            }

            # Khi 2 déséquilibré
            else if(choice == "khi2_desequilibre") {
              r <- pwrss::pwrss.z.2props(
                p1 = p1_val,
                p2 = p2_val,
                power = power_val,
                alpha = alpha_val,
                kappa = kappa,
                verbose = FALSE
              )
              list(n1 = r$n[2], n2 = r$n[1])
            }

            # Fisher
            else {
              n <- exact2x2::ss2x2(
                p0 = p1_val,
                p1 = p2_val,
                power = power_val,
                sig.level = alpha_val,
                alternative = alternative,
                paired = FALSE,
                approx = FALSE
              )$n0
              list(n1 = n, n2 = n)
            }
          }
        )
      ) %>%
      dplyr::mutate(
        n1 = arrondir(map_dbl(tmp, "n1")),
        n2 = arrondir(map_dbl(tmp, "n2")),
        n_total = n1 + n2,
        prop_manquant = missing_prop,
        n_total_ajuste_manquant = ifelse(
          ceiling(n_total / (1 - prop_manquant)) %% 2 == 0,
          ceiling(n_total / (1 - prop_manquant)),
          ceiling(n_total / (1 - prop_manquant)) + 1),
        test=choice,
        kappa=kappa,
        puissance = power
      ) %>%
      dplyr::select(test, puissance, p1, p2, kappa, alpha, prop_manquant, n1, n2, n_total,
                    n_total_ajuste_manquant)
  }

  return(res)
}
