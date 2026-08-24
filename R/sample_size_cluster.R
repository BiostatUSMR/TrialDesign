#' FONCTION ss_cluster()
#'
#' @description
#' Applique un Design Effect (DEFF) au resultat d'un appel prealable a
#' \code{ss_mean_ni()}, \code{ss_mean_sup()}, \code{ss_prop_ni()} ou
#' \code{ss_prop_sup()} pour obtenir les effectifs et le nombre de clusters
#' necessaires dans un essai randomise en cluster (CRT).
#'
#' Workflow typique :
#' \preformatted{
#' res <- ss_mean_ni(mu1 = 10, mu2 = 10, sd = 3, marge = 0.5, choice = "student")
#' ss_cluster(n_ind = res, schema = "crt", m = 25, icc = 0.05)
#' }
#'
#' @param n_ind Data.frame. Objet retourne par \code{ss_mean_ni()}, \code{ss_mean_sup()}, \code{ss_prop_ni()} ou \code{ss_prop_sup()}. La fonction verifie la presence de l'attribut \code{ssdesignr_type}.
#' @param schema Caractere. Schema de cluster :
#' \itemize{
#'   \item \code{"crt"} : CRT parallele simple. DEFF = 1 + (m_eff - 1) * icc.
#'   \item \code{"baseline"} : CRT parallele avec periode baseline. DEFF selon Teerenstra et al. (2012).
#'   \item \code{"sw"} : Stepped-Wedge CRT. DEFF selon Woertman et al. (2013). Requiert \code{k_steps}.
#' }
#' @param m Numerique. Taille de cluster par periode (fixe ou moyenne si cv > 0). Peut etre un vecteur.
#' @param icc Numerique. Intra-cluster correlation coefficient, entre 0 et 1. Peut etre un vecteur.
#' @param cv Numerique. Coefficient de variation des tailles de cluster. Par defaut 0 (tailles fixes). Peut etre un vecteur.
#'   Si cv > 0 : m_eff = m * (1 + cv^2) selon Eldridge et al. (2006).
#' @param k_steps Entier. Nombre de steps du stepped-wedge (>= 2). Requis uniquement si \code{schema = "sw"}. Peut etre un vecteur.
#'
#' @return Le data.frame \code{n_ind} enrichi des colonnes :
#' \itemize{
#'   \item \code{schema} : Schema de cluster utilise.
#'   \item \code{m} : Taille de cluster (ou moyenne).
#'   \item \code{cv} : CV des tailles de cluster.
#'   \item \code{m_eff} : Taille effective = m * (1 + cv^2).
#'   \item \code{icc} : ICC.
#'   \item \code{k_steps} : Nombre de steps. Colonne presente uniquement si \code{schema = "sw"}.
#'   \item \code{deff} : Design Effect calcule.
#'   \item \code{n_total} : Effectif total sous randomisation individuelle (brut).
#'   \item \code{n_total_pdv} : Effectif total individuel avec prise en compte des donnees manquantes.
#'   \item \code{n_cluster} : Effectif total clusterise brut (= n_total * DEFF).
#'   \item \code{n_cluster_pdv} : Effectif total clusterise avec prise en compte des donnees manquantes (= n_total_pdv * DEFF).
#'   \item \code{k_par_bras} : Nombre de clusters par bras.
#'   \item \code{k_total} : Nombre total de clusters.
#' }
#'
#' @importFrom dplyr mutate select any_of all_of
#' @importFrom purrr pmap_dbl
#' @importFrom tidyr crossing
#'
#' @references
#' Donner A, Klar N (2000). Design and Analysis of Cluster Randomization Trials.
#'
#' Eldridge SM et al. (2006). Intracluster correlation and sample size.
#' \emph{Stat Med}, 25(8), 1292-1310.
#'
#' Teerenstra S et al. (2012). A simple sample size formula for analysis of
#' covariance in cluster randomized trials. \emph{Stat Med}, 31(20), 2169-2178.
#'
#' Woertman W et al. (2013). Stepped wedge designs could reduce the required
#' sample size in cluster randomized trials. \emph{J Clin Epidemiol}, 66(7), 752-758.
#'
#' @examples
#' \dontrun{
#' # CRT simple, NI continu
#' res <- ss_mean_ni(
#'   mu1    = 10,
#'   mu2    = 10,
#'   sd     = 3,
#'   marge  = c(0.5, 1.5),
#'   power  = c(0.80, 0.90),
#'   choice = "student"
#' )
#' ss_cluster(
#'   n_ind  = res,
#'   schema = "crt",
#'   m      = c(20, 30, 50),
#'   icc    = c(0.01, 0.05, 0.10)
#' )
#'
#' # Stepped-Wedge, superiorite binaire
#' res <- ss_prop_sup(
#'   p1     = 0.30,
#'   p2     = 0.20,
#'   power  = 0.80,
#'   choice = "khi2"
#' )
#' ss_cluster(
#'   n_ind   = res,
#'   schema  = "sw",
#'   m       = c(20, 30),
#'   icc     = c(0.05, 0.10),
#'   k_steps = c(3, 5)
#' )
#' }
#'
#' @export

ss_cluster <- function(
    n_ind   = NULL,
    schema  = c("crt", "baseline", "sw"),
    m       = NULL,
    icc     = NULL,
    cv      = 0,
    k_steps = NULL
) {

  schema <- match.arg(schema)

  # --- Verification de n_ind ---

  if (is.null(n_ind)) {
    stop(
      "'n_ind' doit etre fourni. Appelez d'abord ss_mean_ni(), ss_mean_sup(), ",
      "ss_prop_ni() ou ss_prop_sup() et passez le resultat a cet argument."
    )
  }

  type_valides <- c("mean_ni", "mean_sup", "prop_ni", "prop_sup")

  if (is.null(attr(n_ind, "ssdesignr_type"))) {
    stop(
      "'n_ind' ne semble pas etre le resultat d'une fonction ssdesignr. ",
      "Verifiez que vous passez bien l'objet retourne par ss_mean_ni(), ",
      "ss_mean_sup(), ss_prop_ni() ou ss_prop_sup()."
    )
  }

  if (!attr(n_ind, "ssdesignr_type") %in% type_valides) {
    stop(sprintf(
      "'n_ind' a un type non reconnu ('%s'). Types acceptes : %s.",
      attr(n_ind, "ssdesignr_type"),
      paste(type_valides, collapse = ", ")
    ))
  }

  # --- Verifications parametres cluster ---

  if (is.null(m) | any(m <= 0)) {
    stop("'m' doit etre fourni et strictement positif.")
  }

  if (is.null(icc) | any(icc < 0) | any(icc >= 1)) {
    stop("'icc' doit etre fourni et compris entre 0 (inclus) et 1 (exclus).")
  }

  if (any(cv < 0)) {
    stop("'cv' doit etre >= 0 (0 = taille fixe).")
  }

  if (schema == "sw") {
    if (is.null(k_steps)) {
      stop("'k_steps' doit etre fourni pour le schema stepped-wedge.")
    }
    if (any(k_steps < 2) | any(k_steps != round(k_steps))) {
      stop("'k_steps' doit etre un entier >= 2.")
    }
  }

  # --- Grille des parametres cluster ---

  if (schema == "sw") {
    grille <- expand.grid(
      m       = m,
      icc     = icc,
      cv      = cv,
      k_steps = k_steps
    )
  } else {
    grille <- expand.grid(
      m   = m,
      icc = icc,
      cv  = cv
    )
    grille$k_steps <- NA_real_
  }

  # --- Produit cartesien n_ind x grille cluster ---

  res_full <- tidyr::crossing(n_ind, grille)

  # --- Calcul DEFF, n_cluster, k ---

  res <- res_full  |>
    dplyr::mutate(

      # Taille de cluster effective (corrigee pour CV si besoin)
      m_eff = .m_eff(m, cv),

      # DEFF selon le schema
      deff = purrr::pmap_dbl(
        list(m, icc, cv, k_steps),
        function(m_i, icc_i, cv_i, ks_i) {
          if (schema == "crt") {
            .deff_crt(m_i, icc_i, cv_i)
          } else if (schema == "baseline") {
            .deff_baseline(m_i, icc_i, cv_i)
          } else {
            .deff_sw(m_i, icc_i, ks_i, cv_i)
          }
        }
      ),

      # N clusterise brut (base sur n_total sans manquants)
      n_cluster     = ceiling(n_total * deff),

      # N clusterise ajuste (base sur n_total_pdv deja ajuste aux manquants)
      n_cluster_pdv = ceiling(n_total_pdv * deff),

      # Nombre de clusters par bras et total (base sur n_cluster_pdv)
      k_par_bras    = ceiling(n_cluster_pdv / 2 / m_eff),
      k_total       = k_par_bras * 2,

      schema        = schema

    )

  # --- Selection des colonnes ---
  # k_steps n'est inclus que si le schema l'utilise reellement (sw),
  # pour eviter une colonne systematiquement vide dans les rapports Word
  # pour les schemas "crt" et "baseline".

  colonnes <- c(
    "schema",
    "test", "puissance", "mu1", "mu2", "sd", "sd1", "sd2", "p1", "p2", "marge",
    "alpha", "kappa",
    "missing_prop",
    "m", "cv", "m_eff", "icc"
  )

  if (schema == "sw") {
    colonnes <- c(colonnes, "k_steps")
  }

  colonnes <- c(
    colonnes,
    "deff", "n_total", "n_total_pdv", "n_cluster", "n_cluster_pdv",
    "k_par_bras", "k_total"
  )

  res <- dplyr::select(res, dplyr::any_of(colonnes))

  # --- Labels pour affichage (ss_report) ---

  labels_cluster <- list(
    schema        = "Schema",
    test          = "Test",
    puissance     = "Puissance",
    mu1           = "Moyenne 1",
    mu2           = "Moyenne 2",
    sd            = "Ecart-type commun",
    sd1           = "Ecart-type 1",
    sd2           = "Ecart-type 2",
    p1            = "Proportion 1",
    p2            = "Proportion 2",
    marge         = "Marge NI",
    alpha         = "Alpha",
    kappa         = "Ratio N1/N2",
    missing_prop  = "% d.m",
    m             = "Taille de cluster (m)",
    cv            = "CV tailles de cluster",
    m_eff         = "Taille effective (m_eff)",
    icc           = "ICC",
    k_steps       = "Nb steps (SW)",
    deff          = "Design Effect",
    n_total       = "N (individuel)",
    n_total_pdv   = "N (individuel) - avec d.m",
    n_cluster     = "N (clusterise)",
    n_cluster_pdv = "N (clusterise) - avec d.m",
    k_par_bras    = "Clusters / bras",
    k_total       = "Clusters (total)"
  )

  labels_cluster <- labels_cluster[names(labels_cluster) %in% names(res)]
  res <- labelled::set_variable_labels(res, !!!labels_cluster)

  # Reattache le type d'origine + marqueur "cluster" pour ss_report()
  attr(res, "ssdesignr_type")    <- attr(n_ind, "ssdesignr_type")
  attr(res, "ssdesignr_cluster") <- TRUE

  return(res)
}
