#' Calcul de la taille d'\u00e9chantillon pour la comparaison de deux moyennes
#'
#' @description
#' Calcule la taille d'\u00e9chantillon n\u00e9cessaire pour comparer deux moyennes
#' selon diff\u00e9rents tests statistiques (Student, Welch, Wilcoxon).
#' Supporte les groupes \u00e9quilibr\u00e9s et d\u00e9s\u00e9quilibr\u00e9s, et permet d'ajuster
#' l'effectif sur un taux de donn\u00e9es manquantes.
#'
#' @param mu1 Vecteur num\u00e9rique. Moyenne(s) attendue(s) dans le groupe 1.
#' @param mu2 Vecteur num\u00e9rique. Moyenne(s) attendue(s) dans le groupe 2.
#' @param sd Num\u00e9rique. \u00c9cart-type commun aux deux groupes.
#'   Requis pour les tests de Student (\code{"test_student_equilibre"} et
#'   \code{"test_student_desequilibre"}).
#' @param sd1 Num\u00e9rique. \u00c9cart-type du groupe 1.
#'   Requis pour les tests de Welch et Wilcoxon.
#' @param sd2 Num\u00e9rique. \u00c9cart-type du groupe 2.
#'   Requis pour les tests de Welch et Wilcoxon.
#' @param power Vecteur num\u00e9rique. Puissance(s) souhait\u00e9e(s), entre 0 et 1.
#'   Par d\u00e9faut \code{0.80}.
#' @param alpha Vecteur num\u00e9rique. Niveau(x) de significativit\u00e9, entre 0 et 1.
#'   Par d\u00e9faut \code{0.05}.
#' @param kappa Num\u00e9rique. Ratio n2/n1 pour les designs d\u00e9s\u00e9quilibr\u00e9s.
#'   Par d\u00e9faut \code{1} (groupes \u00e9quilibr\u00e9s).
#' @param missing_rate Vecteur num\u00e9rique. Taux de donn\u00e9es manquantes attendu,
#'   entre 0 et 1. Par d\u00e9faut \code{0}.
#' @param nsim Entier. Nombre de simulations pour le test de Wilcoxon.
#'   Par d\u00e9faut \code{10000}.
#' @param alternative Caract\u00e8re. Type de test : \code{"two.sided"} (par d\u00e9faut),
#'   \code{"less"} ou \code{"greater"}.
#' @param choice Caract\u00e8re. Test statistique \u00e0 utiliser. Valeurs possibles :
#'   \itemize{
#'     \item \code{"test_student_equilibre"} : test de Student, groupes \u00e9quilibr\u00e9s
#'     \item \code{"test_student_desequilibre"} : test de Student, groupes d\u00e9s\u00e9quilibr\u00e9s
#'     \item \code{"test_welch_equilibre"} : test de Welch, groupes \u00e9quilibr\u00e9s
#'     \item \code{"test_welch_desequilibre"} : test de Welch, groupes d\u00e9s\u00e9quilibr\u00e9s
#'     \item \code{"test_wilcoxon"} : test de Wilcoxon-Mann-Whitney (simulation)
#'   }
#'
#' @return Un data.frame contenant une ligne par combinaison de param\u00e8tres,
#'   avec les colonnes :
#'   \itemize{
#'     \item \code{test} : test utilis\u00e9
#'     \item \code{puissance} : puissance utilis\u00e9e
#'     \item \code{mu1}, \code{mu2} : moyennes
#'     \item \code{alpha} : niveau de significativit\u00e9
#'     \item \code{kappa} : ratio d'allocation
#'     \item \code{prop_manquant} : taux de donn\u00e9es manquantes
#'     \item \code{n1}, \code{n2} : effectifs par groupe (arrondis \u00e0 l'entier sup\u00e9rieur)
#'     \item \code{n_total} : effectif total brut
#'     \item \code{n_total_ajuste_manquant} : effectif total ajust\u00e9 sur le taux de manquants
#'   }
#'
#' @details
#' La fonction g\u00e9n\u00e8re automatiquement toutes les combinaisons possibles des
#' vecteurs d'arguments via \code{expand.grid()}, et exclut les combinaisons
#' o\u00f9 \code{mu1 == mu2} (pas de diff\u00e9rence \u00e0 d\u00e9tecter).
#' Les effectifs sont syst\u00e9matiquement arrondis \u00e0 l'entier sup\u00e9rieur.
#' L'ajustement sur les donn\u00e9es manquantes est calcul\u00e9 comme :
#' \code{ceiling(n_total / (1 - missing_rate) / 2) * 2}.
#'
#' @examples
#' \dontrun{
#' # Student \u00e9quilibr\u00e9 avec plusieurs sc\u00e9narios
#' sample_size_mean(
#'   mu1 = c(55, 60),
#'   mu2 = c(50, 52),
#'   sd  = 10,
#'   power = c(0.80, 0.90),
#'   missing_rate = 0.10,
#'   choice = "test_student_equilibre"
#' )
#'
#' # Welch d\u00e9s\u00e9quilibr\u00e9
#' sample_size_mean(
#'   mu1   = 60,
#'   mu2   = 50,
#'   sd1   = 10,
#'   sd2   = 15,
#'   kappa = 2,
#'   power = 0.80,
#'   choice = "test_welch_desequilibre"
#' )
#' }
#'
#' @importFrom stats power.t.test
#' @importFrom dplyr mutate select filter
#' @importFrom purrr pmap map_dbl
#' @export
#'


sample_size_mean <- function(
    mu1 = NULL, mu2 = NULL,
    sd = NULL,
    sd1 = NULL, sd2 = NULL,
    power = 0.80,
    alpha = 0.05,
    kappa = 1,
    missing_rate = 0,
    nsim = 10000,
    alternative = "two.sided",
    choice = c("test_student_equilibre",
               "test_student_desequilibre",
               "test_welch_equilibre",
               "test_welch_desequilibre",
               "test_wilcoxon")
){

  choice <- match.arg(choice)

  if(is.null(mu1) | is.null(mu2)){
    stop("Les moyennes 'mu1' et 'mu2' doivent être fournies.")
  }

  if(choice %in% c("test_student_equilibre","test_student_desequilibre")){
    if(is.null(sd)){
      stop(" 'sd' doit être fourni pour les tests Student.")
    }
    if(!is.null(sd1) | !is.null(sd2)){
      warning(" Pour Student, utilisez un écart-type commun 'sd'.")
    }
  }

  if(choice %in% c("test_welch_equilibre","test_welch_desequilibre")){
    if(is.null(sd1) | is.null(sd2)){
      stop(" 'sd1' et 'sd2' doivent être fournis pour les tests de Welch.")
    }
    if(!is.null(sd) & sd1==sd2){
      warning(" Vous avez fourni 'sd' identique pour Welch. Vérifiez que vous souhaitez des variances différentes.")
    }
  }

  arrondir <- function(x) ceiling(as.numeric(x))

  alt_pwrss <- ifelse(alternative == "two.sided","not equal",alternative)

  params <- expand.grid(
    mu1 = mu1,
    mu2 = mu2,
    power = power,
    alpha = alpha,
    missing_prop = missing_rate
  ) %>%
    dplyr::filter(mu1 != mu2)

  res <- params %>%
    dplyr::mutate(
      tmp = purrr::pmap(
        list(mu1, mu2, power, alpha),
        function(mu1, mu2, power, alpha){

          if(choice == "test_student_equilibre"){
            n <- stats::power.t.test(
              n = NULL,
              delta = abs(mu1-mu2),
              sd = sd,
              sig.level = alpha,
              power = power,
              type = "two.sample",
              alternative = alternative
            )$n
            return(list(n1=n, n2=n))
          }

          if(choice == "test_student_desequilibre"){
            r <- pwrss::pwrss.t.2means(
              mu1 = mu1,
              mu2 = mu2,
              sd1 = sd,
              sd2 = sd,
              power = power,
              alpha = alpha,
              kappa = kappa,
              alternative = alt_pwrss,
              verbose = FALSE
            )
            return(list(n1=r$n[1], n2=r$n[2]))
          }

          if(choice == "test_welch_equilibre"){
            n <- MKpower::power.welch.t.test(
              n = NULL,
              delta = abs(mu1-mu2),
              sd1 = sd1,
              sd2 = sd2,
              power = power,
              sig.level = alpha,
              alternative = alternative
            )$n
            return(list(n1=n, n2=n))
          }

          if(choice == "test_welch_desequilibre"){
            r <- pwrss::pwrss.t.2means(
              mu1 = mu1,
              mu2 = mu2,
              sd1 = sd1,
              sd2 = sd2,
              power = power,
              alpha = alpha,
              welch.df = TRUE,
              kappa = kappa,
              alternative = alt_pwrss,
              verbose = FALSE
            )
            return(list(n1=r$n[1], n2=r$n[2]))
          }

          if(choice == "test_wilcoxon"){
            set.seed(123)
            x <- rnorm(nsim, mean = mu1, sd = sd1)
            y <- rnorm(nsim, mean = mu2, sd = sd2)

            r <- WMWssp::WMWssp_minimize(
              x = x,
              y = y,
              alpha = alpha,
              power = power,
              simulation = TRUE,
              nsim = nsim
            )

            n1 <- r$result["n1 rounded", "Results"]
            n2 <- r$result["n2 rounded", "Results"]

            return(list(n1 = n1, n2 = n2))
          }

        }
      )
    ) %>%
    dplyr::mutate(
      n1 = arrondir(purrr::map_dbl(tmp, ~.x$n1)),
      n2 = arrondir(purrr::map_dbl(tmp, ~.x$n2)),
      n_total = n1+n2,
      prop_manquant = missing_prop,
      n_total_ajuste_manquant = ceiling(n_total/(1-missing_prop)/2)*2,
      puissance = power,
      test = choice,
      kappa = kappa
    ) %>%
    dplyr::select(
      test,
      puissance,
      mu1,
      mu2,
      alpha,
      kappa,
      prop_manquant,
      n1,
      n2,
      n_total,
      n_total_ajuste_manquant
    )

  return(res)
}

