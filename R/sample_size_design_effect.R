
###########################################################
# FONCTION INTERNE .m_eff()
#

.m_eff <- function(m, cv = 0) {
  if (any(cv < 0)) stop("'cv' doit etre >= 0.")
  m * (1 + cv^2)
}


###########################################################
# FONCTION INTERNE .deff_crt()
#

.deff_crt <- function(m, icc, cv = 0) {
  if (any(icc < 0) | any(icc >= 1)) stop("'icc' doit etre compris entre 0 (inclus) et 1 (exclus).")
  if (any(m <= 0))                   stop("'m' doit etre strictement positif.")
  me <- .m_eff(m, cv)
  1 + (me - 1) * icc
}


###########################################################
# FONCTION INTERNE .deff_baseline()
#

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
