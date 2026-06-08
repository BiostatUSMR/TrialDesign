
###########################################################
# FONCTION INTERNE .m_eff()
#
# Calcule la taille de cluster effective en presence de variabilite
# de taille de cluster (CV > 0).
#
# Arguments :
#   m  : Taille de cluster (fixe ou moyenne si variable).
#   cv : Coefficient de variation des tailles (defaut = 0 -> fixe).
#
# Formule : m_eff = m * (1 + CV^2)
# Reference : Eldridge SM et al. (2006). Stat Med. 25(8):1292-310.

.m_eff <- function(m, cv = 0) {
  if (any(cv < 0)) stop("'cv' doit etre >= 0.")
  m * (1 + cv^2)
}


###########################################################
# FONCTION INTERNE .deff_crt()
#
# Calcule le Design Effect pour un CRT parallele simple.
#
# Arguments :
#   m   : Taille de cluster (fixe ou moyenne si cv > 0).
#   icc : Intra-cluster correlation coefficient (entre 0 et 1).
#   cv  : Coefficient de variation des tailles de cluster (defaut = 0).
#
# Formule : DEFF = 1 + (m_eff - 1) * icc
# Reference : Donner A, Klar N (2000).

.deff_crt <- function(m, icc, cv = 0) {
  if (any(icc < 0) | any(icc >= 1)) stop("'icc' doit etre compris entre 0 (inclus) et 1 (exclus).")
  if (any(m <= 0))                   stop("'m' doit etre strictement positif.")
  me <- .m_eff(m, cv)
  1 + (me - 1) * icc
}


###########################################################
# FONCTION INTERNE .deff_baseline()
#
# Calcule le Design Effect pour un CRT parallele avec periode baseline.
#
# Arguments :
#   m   : Taille de cluster par periode (fixe ou moyenne si cv > 0).
#   icc : Intra-cluster correlation coefficient (entre 0 et 1).
#   cv  : Coefficient de variation des tailles de cluster (defaut = 0).
#
# Formule :
#   DE_BA = 2 * [1 + (m_eff - 1) * icc] * [1 - (m_eff * icc / (1 + (m_eff - 1) * icc))^2]
# Reference : Teerenstra S et al. (2012). Stat Med. 31(20):2169-78.

.deff_baseline <- function(m, icc, cv = 0) {
  if (any(icc < 0) | any(icc >= 1)) stop("'icc' doit etre compris entre 0 (inclus) et 1 (exclus).")
  if (any(m <= 0))                   stop("'m' doit etre strictement positif.")
  me     <- .m_eff(m, cv)
  deff_p <- 1 + (me - 1) * icc
  R      <- (me * icc) / deff_p
  2 * deff_p * (1 - R^2)
}


###########################################################
# FONCTION INTERNE .deff_sw()
#
# Calcule le Design Effect pour un Stepped-Wedge CRT.
#
# Arguments :
#   m   : Taille de cluster par periode (fixe ou moyenne si cv > 0).
#   icc : Intra-cluster correlation coefficient (entre 0 et 1).
#   k   : Nombre de steps (entier >= 2).
#   cv  : Coefficient de variation des tailles de cluster (defaut = 0).
#
# Formule (Woertman 2013) :
#   DE_SW = (k+1) * [1 + icc*(k*m_eff + m_eff - 1)] /
#                   [1 + icc*(k*m_eff/2 + m_eff - 1)] *
#           3*(1-icc) / [2*(k - 1/k)]
# Reference : Woertman W et al. (2013). J Clin Epidemiol. 66(7):752-8.

.deff_sw <- function(m, icc, k, cv = 0) {
  if (any(icc < 0) | any(icc >= 1))    stop("'icc' doit etre compris entre 0 (inclus) et 1 (exclus).")
  if (any(m <= 0))                      stop("'m' doit etre strictement positif.")
  if (any(k < 2) | any(k != round(k))) stop("'k' (nombre de steps) doit etre un entier >= 2.")
  me <- .m_eff(m, cv)
  A  <- 1 + icc * (k * me + me - 1)
  B  <- 1 + icc * (k * me / 2 + me - 1)
  C  <- 3 * (1 - icc)
  D  <- 2 * (k - 1 / k)
  (k + 1) * (A / B) * (C / D)
}
