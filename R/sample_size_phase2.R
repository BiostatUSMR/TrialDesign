#' Calcul de la taille d'échantillon pour un essai de phase II à un bras
#'
#' @description
#' Calcule la taille d'échantillon nécessaire pour un essai de phase II à un
#' bras selon les méthodes de A'Hern ou de Fleming. L'objectif est de tester
#' si la proportion de succès attendue est suffisamment élevée pour justifier
#' le passage en phase III.
#'
#' La règle de décision est définie par le couple \code{(n_patient, n_succes)} :
#' si le nombre de succès observés est supérieur ou égal à \code{n_succes},
#' l'hypothèse nulle H0 est rejetée.
#'
#' @param p0 Numérique. Proportion de succès sous l'hypothèse nulle (taux
#'   inacceptable). Doit être compris entre 0 et 1, et strictement inférieur
#'   à \code{p1}. Peut être un vecteur pour tester plusieurs scénarios.
#' @param p1 Numérique. Proportion de succès sous l'hypothèse alternative
#'   (taux attendu). Doit être compris entre 0 et 1, et strictement supérieur
#'   à \code{p0}. Doit avoir la même longueur que \code{p0}.
#' @param alpha Numérique. Niveau de signification (erreur de type I).
#'   Par défaut \code{0.05}.
#' @param power Numérique. Puissance souhaitée (1 - erreur de type II).
#'   Par défaut \code{0.80}.
#' @param method Caractère. Méthode de calcul : \code{"ahern"} ou
#'   \code{"fleming"}.
#'   \itemize{
#'     \item \code{"ahern"} : explore tous les couples \code{(n, r)} dans la
#'       plage \code{[10, nmax]} et retient le plus petit \code{n} satisfaisant
#'       les contraintes d'erreur de type I et de puissance.
#'     \item \code{"fleming"} : utilise la fonction \code{ph2single()} du
#'       package \code{clinfun}.
#'   }
#' @param missing_rate Numérique. Proportion attendue de données manquantes,
#'   utilisée pour ajuster la taille d'échantillon. Doit être compris entre
#'   0 (inclus) et 1 (exclu). Par défaut \code{0} (aucun ajustement).
#' @param nmax Entier. Taille maximale explorée pour la méthode A'Hern.
#'   Ignoré si \code{method = "fleming"}. Par défaut \code{200}.
#'
#' @return Un data.frame avec une ligne par combinaison de paramètres et les
#'   colonnes suivantes :
#'   \describe{
#'     \item{methode}{Méthode utilisée (\code{"ahern"} ou \code{"fleming"})}
#'     \item{p0}{Proportion de succès sous H0}
#'     \item{p1}{Proportion de succès sous H1}
#'     \item{alpha}{Niveau de signification utilisé}
#'     \item{power}{Puissance utilisée}
#'     \item{missing_prop}{Proportion de données manquantes}
#'     \item{n_patient}{Taille d'échantillon brute (sans ajustement)}
#'     \item{n_ajuste}{Taille d'échantillon ajustée pour les données manquantes :
#'       \eqn{n_{ajuste} = \lceil n_{patient} / (1 - missing\_prop) \rceil}}
#'     \item{n_succes}{Nombre minimal de succès requis pour rejeter H0}
#'   }
#'
#' @details
#' Les paramètres \code{p0} et \code{p1} peuvent être des vecteurs de même
#' longueur pour évaluer plusieurs scénarios simultanément. La fonction
#' construit toutes les combinaisons possibles via \code{expand.grid()}.
#'
#' Pour la méthode A'Hern, si aucune solution n'est trouvée dans la plage
#' \code{[10, nmax]}, un avertissement est émis et \code{NA} est retourné
#' pour le scénario concerné. Dans ce cas, augmentez \code{nmax}.
#'
#' Toutes les tailles d'échantillon sont arrondies à l'entier supérieur
#' (\code{ceiling}).
#'
#'
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap map_dbl
#' @importFrom stats pbinom
#' @importFrom clinfun ph2single
#'
#' @examples
#' # Méthode A'Hern — deux scénarios
#' sample_size_phase2(
#'   p0           = c(0.10, 0.20),
#'   p1           = c(0.30, 0.40),
#'   method       = "ahern",
#'   missing_rate = 0.10,
#'   nmax         = 200
#' )
#'
#' # Méthode Fleming — avec ajustement pour données manquantes
#' sample_size_phase2(
#'   p0           = c(0.10, 0.20),
#'   p1           = c(0.30, 0.40),
#'   method       = "fleming",
#'   missing_rate = 0.10
#' )
#'
#' # Comparaison des deux méthodes sur un même scénario
#' sample_size_phase2(p0 = 0.20, p1 = 0.40, method = "ahern")
#' sample_size_phase2(p0 = 0.20, p1 = 0.40, method = "fleming")
#'
#' @export
sample_size_phase2 <- function(
    p0,
    p1,
    alpha        = 0.05,
    power        = 0.80,
    method       = c("ahern", "fleming"),
    missing_rate = 0,
    nmax         = 200
) {

  method <- match.arg(method)

  # --- Vérifications ---
  if (any(c(p0, p1) < 0 | c(p0, p1) > 1))
    stop("p0 et p1 doivent \u00eatre compris entre 0 et 1.")
  if (any(p0 >= p1))
    stop("p0 doit \u00eatre strictement inf\u00e9rieur \u00e0 p1.")
  if (missing_rate < 0 | missing_rate >= 1)
    stop("missing_rate doit \u00eatre compris entre 0 et 1.")

  params <- expand.grid(
    p0           = p0,
    p1           = p1,
    alpha        = alpha,
    power        = power,
    missing_prop = missing_rate
  )

  res <- dplyr::mutate(
    params,
    tmp = purrr::pmap(
      list(p0, p1, alpha, power),
      function(p0, p1, alpha, power) {

        # -------- A'Hern --------
        if (method == "ahern") {
          solution <- NULL
          for (n_patient in 10:nmax) {
            for (n_succes in 0:n_patient) {
              type1 <- 1 - stats::pbinom(n_succes - 1, n_patient, p0)
              type2 <- 1 - stats::pbinom(n_succes - 1, n_patient, p1)
              if (type1 <= alpha & type2 >= power) {
                solution <- list(n_patient = n_patient, n_succes = n_succes)
                break
              }
            }
            if (!is.null(solution)) break
          }

          if (is.null(solution)) {
            warning("Aucune solution trouv\u00e9e pour p0=", p0,
                    " p1=", p1, " avec nmax=", nmax,
                    ". Augmentez nmax.")
            return(list(n_patient = NA_real_, n_succes = NA_real_))
          } else {
            return(solution)
          }
        }

        # -------- Fleming --------
        if (method == "fleming") {
          res <- clinfun::ph2single(pu = p0, pa = p1,
                                    ep1 = alpha, ep2 = 1 - power)
          return(list(n_patient = res$n[1], n_succes = res$r[1]))
        }
      }
    )
  )

  res <- dplyr::mutate(
    res,
    n_patient = purrr::map_dbl(tmp, ~ .x$n_patient),
    n_succes  = purrr::map_dbl(tmp, ~ .x$n_succes),
    n_ajuste  = ceiling(n_patient / (1 - missing_prop)),
    methode   = method
  )

  res <- dplyr::select(
    res,
    methode, p0, p1, alpha, power, missing_prop,
    n_patient, n_ajuste, n_succes
  )

  return(res)
}
