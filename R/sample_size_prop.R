#' FONCTION sample_size_prop
#'
#'@description
#'Calcule la taille d'échantillon nécessaire pour comparer deux proportions selon différents tests statistiques.
#'
#' @param p1 Proportion à comparer (numériques entre 0 et 1)
#' @param p2 Proportion à comparer (numériques entre 0 et 1)
#' @param power Puissance souhaitée (défaut = 0.80, entre 0 et 1)
#' @param alpha Niveau de significativité (défaut = 0.05, entre 0 et 1)
#' @param kappa Ratio n2/n1 pour déséquilibre (défaut = 1)
#' @param p01 Pour McNemar, proportion de paires discordantes
#' @param p10 Pour McNemar, proportion de paires discordantes
#' @param missing_rate Taux de données manquantes (défaut = 0)
#' @param alternative "two.sided", "less", ou "greater"
#' @param choice Test choisi parmi :
#'            - "khi2_equilibre" : Khi-2 groupes équilibrés
#'            - "khi2_desequilibre" : Khi-2 avec déséquilibre
#'            - "fisher" : Test exact de Fisher
#'            - "mcnemar" : Test de McNemar apparié
#'@import purrr, pwrss, exact2x2, dplyr
#'
#' @returns
#'
#'
#' @examples
#' \dontrun{
#' sample_size_prop(
#'   p1 = c(0.25, 0.10, 0.30),
#'   p2 = c(0.26, 0.50, 0.40),
#'   power = c(0.80, 0.90),
#'   missing_rate = c(0.05,0.10),
#'   choice = "khi2_equilibre")
#'   }
#'
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
