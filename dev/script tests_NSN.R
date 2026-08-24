#==============================================================================
# SCRIPT DE TEST - Fonctions de calcul NSN (Package TrialDesign)
#
# Objectif : vérifier que chaque fonction de calcul tourne sans erreur,
# pour chaque valeur de `choice`, et que le résultat s'exporte correctement
# en Word via ss_report(). Vérifie aussi ss_cluster() sur chaque type de
# résultat compatible.
#
# Utilisation : source ce script depuis la racine de votre projet (ou avec
# devtools::load_all() déjà exécuté). Les .docx sont écrits dans un
# sous-dossier "test_outputs/" du répertoire de travail courant.
#==============================================================================

setwd(here::here())

devtools::load_all()

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)

out_dir <- file.path(old_wd, paste0("test_outputs_", format(Sys.Date(), "%Y%m%d")))
if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
dir.create(out_dir)
setwd(out_dir)

test_log <- data.frame(
  test = character(0),
  statut = character(0),
  message = character(0),
  stringsAsFactors = FALSE
)

log_result <- function(test, statut, message = "") {
  test_log <<- rbind(test_log, data.frame(test = test, statut = statut, message = message))
  cat(sprintf("[%s] %s %s\n", statut, test, ifelse(message == "", "", paste0("- ", message))))
}

run_test <- function(label, expr) {
  res <- tryCatch(
    {
      val <- eval(expr)
      log_result(label, "OK")
      val
    },
    error = function(e) {
      log_result(label, "ECHEC", conditionMessage(e))
      NULL
    },
    warning = function(w) {
      # On laisse passer les warnings volontaires (ex: alpha > 0.05 en NI)
      # mais on les journalise pour info
      val <- withCallingHandlers(
        eval(expr),
        warning = function(ww) invokeRestart("muffleWarning")
      )
      log_result(label, "OK (avec warning)", conditionMessage(w))
      val
    }
  )
  res
}

check_docx <- function(path) {
  if (file.exists(path) && file.size(path) > 0) {
    log_result(paste("Fichier généré :", basename(path)), "OK")
  } else {
    log_result(paste("Fichier généré :", basename(path)), "ECHEC", "fichier absent ou vide")
  }
}


#==============================================================================
# 1. ss_mean_sup() -- toutes les valeurs de `choice`
#==============================================================================

cat("\n===== ss_mean_sup() =====\n")

res_mean_sup_student <- run_test("ss_mean_sup - student", quote(
  ss_mean_sup(
    mu1    = c(55, 60),
    mu2    = 50,
    sd     = 10,
    power  = c(0.80, 0.90),
    alpha  = 0.05,
    kappa  = 1,
    sided  = 2,
    choice = "student"
  )
))

res_mean_sup_welch <- run_test("ss_mean_sup - welch (desequilibre)", quote(
  ss_mean_sup(
    mu1    = c(55, 60),
    mu2    = 50,
    sd1    = 10,
    sd2    = 15,
    power  = 0.80,
    kappa  = 1/2,
    choice = "welch"
  )
))

res_mean_sup_wilcoxon <- run_test("ss_mean_sup - wilcoxon", quote(
  ss_mean_sup(
    mu1    = 55,
    mu2    = 50,
    sd1    = 10,
    sd2    = 10,
    power  = 0.80,
    nsim   = 2000,     # reduit pour accelerer le test
    seed   = 123,
    choice = "wilcoxon"
  )
))

# Cas limite : missing_prop vectoriel
res_mean_sup_pdv <- run_test("ss_mean_sup - missing_prop vectoriel", quote(
  ss_mean_sup(
    mu1          = 55,
    mu2          = 50,
    sd           = 10,
    missing_prop = c(0, 0.1, 0.2),
    choice       = "student"
  )
))

# Cas d'erreur attendu : mu1 == mu2 seul (doit filtrer, pas planter)
res_mean_sup_egal <- run_test("ss_mean_sup - mu1 == mu2 (filtrage attendu)", quote(
  ss_mean_sup(mu1 = 50, mu2 = 50, sd = 10, choice = "student")
))
if (!is.null(res_mean_sup_egal) && nrow(res_mean_sup_egal) == 0) {
  log_result("ss_mean_sup - mu1==mu2 -> 0 ligne", "OK", "comportement attendu")
} else if (is.null(res_mean_sup_egal)) {
  log_result("ss_mean_sup - mu1==mu2", "ECHEC", "a plante au lieu de filtrer/retourner 0 ligne")
}


#==============================================================================
# 2. ss_mean_ni() -- toutes les valeurs de `choice`
#==============================================================================

cat("\n===== ss_mean_ni() =====\n")

res_mean_ni_student <- run_test("ss_mean_ni - student", quote(
  ss_mean_ni(
    mu1    = 10,
    mu2    = 10,
    sd     = 3,
    marge  = c(0.5, 1.5),
    power  = c(0.80, 0.90),
    alpha  = 0.025,
    choice = "student"
  )
))

res_mean_ni_welch <- run_test("ss_mean_ni - welch (desequilibre)", quote(
  ss_mean_ni(
    mu1    = 10,
    mu2    = 10,
    sd1    = 3,
    sd2    = 4,
    marge  = 1,
    kappa  = 2,
    choice = "welch"
  )
))


#==============================================================================
# 3. ss_prop_sup() -- toutes les valeurs de `choice`
#==============================================================================

cat("\n===== ss_prop_sup() =====\n")

res_prop_sup_khi2 <- run_test("ss_prop_sup - khi2", quote(
  ss_prop_sup(
    p1           = c(0.20, 0.30),
    p2           = 0.70,
    power        = c(0.80, 0.90),
    missing_prop = 0.05,
    kappa        = 1,
    choice       = "khi2",
    sided        = 2
  )
))

res_prop_sup_fisher <- run_test("ss_prop_sup - fisher", quote(
  ss_prop_sup(
    p1     = 0.20,
    p2     = 0.70,
    power  = 0.80,
    kappa  = 0.5,
    choice = "fisher",
    sided  = 2
  )
))

res_prop_sup_mcnemar <- run_test("ss_prop_sup - mcnemar", quote(
  ss_prop_sup(
    p01    = 0.10,
    p10    = 0.20,
    power  = 0.2,
    choice = "mcnemar"
  )
))

# Cas limite : direction inverse (p1 < p2) pour verifier la derivation
# automatique de `alternative` a partir de `sided`
res_prop_sup_fisher_inv <- run_test("ss_prop_sup - fisher (p1 < p2, sided=1)", quote(
  ss_prop_sup(
    p1     = 0.20,
    p2     = 0.50,
    power  = 0.80,
    choice = "fisher",
    sided  = 1
  )
))

# Verification p01/p10 negatifs -> doit stopper proprement
test_p01_negatif <- tryCatch(
  {
    ss_prop_sup(p01 = -0.1, p10 = 0.2, choice = "mcnemar")
    log_result("ss_prop_sup - p01 negatif (doit echouer proprement)", "ECHEC", "aucune erreur levee")
  },
  error = function(e) {
    log_result("ss_prop_sup - p01 negatif (doit echouer proprement)", "OK", conditionMessage(e))
  }
)


#==============================================================================
# 4. ss_prop_ni()
#==============================================================================

cat("\n===== ss_prop_ni() =====\n")

res_prop_ni <- run_test("ss_prop_ni - khi2", quote(
  ss_prop_ni(
    p1     = c(0.20, 0.30),
    p2     = 0.70,
    marge  = c(0.01, 0.15, 0.30),
    power  = c(0.80, 0.90),
    alpha  = 0.025
  )
))

res_prop_ni_desequilibre <- run_test("ss_prop_ni - khi2 desequilibre + pdv", quote(
  ss_prop_ni(
    p1           = 0.20,
    p2           = 0.70,
    marge        = 0.15,
    kappa        = 2,
    missing_prop = c(0.05, 0.10)
  )
))


#==============================================================================
# 5. ss_cluster() -- sur chaque type de resultat compatible
#==============================================================================

cat("\n===== ss_cluster() =====\n")

res_cluster_crt <- run_test("ss_cluster - crt (sur mean_ni/student)", quote(
  ss_cluster(
    n_ind  = res_mean_ni_student,
    schema = "crt",
    m      = c(20, 30, 50),
    icc    = c(0.01, 0.05, 0.10)
  )
))

res_cluster_baseline <- run_test("ss_cluster - baseline (sur mean_sup/student)", quote(
  ss_cluster(
    n_ind  = res_mean_sup_student,
    schema = "baseline",
    m      = 25,
    icc    = 0.05
  )
))

res_cluster_sw <- run_test("ss_cluster - stepped-wedge (sur prop_sup/khi2)", quote(
  ss_cluster(
    n_ind   = res_prop_sup_khi2,
    schema  = "sw",
    m       = c(20, 30),
    icc     = c(0.05, 0.10),
    k_steps = c(3, 5)
  )
))

res_cluster_cv <- run_test("ss_cluster - crt avec cv > 0", quote(
  ss_cluster(
    n_ind  = res_prop_ni,
    schema = "crt",
    m      = 25,
    icc    = 0.05,
    cv     = 0.3
  )
))

# Verification : n_ind invalide (data.frame quelconque) -> doit stopper
test_n_ind_invalide <- tryCatch(
  {
    ss_cluster(n_ind = data.frame(x = 1), schema = "crt", m = 25, icc = 0.05)
    log_result("ss_cluster - n_ind invalide (doit echouer proprement)", "ECHEC", "aucune erreur levee")
  },
  error = function(e) {
    log_result("ss_cluster - n_ind invalide (doit echouer proprement)", "OK", conditionMessage(e))
  }
)

# Verification que les labels et attributs sont bien transmis
if (!is.null(res_cluster_crt)) {
  a_type <- attr(res_cluster_crt, "ssdesignr_type")
  a_clus <- attr(res_cluster_crt, "ssdesignr_cluster")
  if (identical(a_type, "mean_ni") && isTRUE(a_clus)) {
    log_result("ss_cluster - attributs ssdesignr_type/cluster", "OK")
  } else {
    log_result("ss_cluster - attributs ssdesignr_type/cluster", "ECHEC",
               paste("type =", a_type, "/ cluster =", a_clus))
  }

  labs <- labelled::var_label(res_cluster_crt)
  n_labs_manquants <- sum(vapply(labs, is.null, logical(1)))
  if (n_labs_manquants == 0) {
    log_result("ss_cluster - labels sur toutes les colonnes", "OK")
  } else {
    log_result("ss_cluster - labels sur toutes les colonnes", "ECHEC",
               paste(n_labs_manquants, "colonne(s) sans label"))
  }
}


#==============================================================================
# 5bis. sample_size_phase2() -- A'Hern et Fleming
#==============================================================================

cat("\n===== sample_size_phase2() =====\n")

res_phase2_ahern <- run_test("sample_size_phase2 - ahern", quote(
  sample_size_phase2(
    p0     = c(0.10, 0.20),
    p1     = c(0.30, 0.40),
    alpha  = 0.05,
    power  = 0.80,
    method = "ahern",
    nmax   = 100
  )
))

res_phase2_fleming <- run_test("sample_size_phase2 - fleming", quote(
  sample_size_phase2(
    p0     = c(0.10, 0.20),
    p1     = c(0.30, 0.40),
    alpha  = 0.05,
    power  = 0.80,
    method = "fleming"
  )
))

# Cas limite : nmax trop petit -> doit stopper proprement
test_nmax_invalide <- tryCatch(
  {
    sample_size_phase2(p0 = 0.1, p1 = 0.3, method = "ahern", nmax = 5)
    log_result("sample_size_phase2 - nmax < 10 (doit echouer proprement)", "ECHEC", "aucune erreur levee")
  },
  error = function(e) {
    log_result("sample_size_phase2 - nmax < 10 (doit echouer proprement)", "OK", conditionMessage(e))
  }
)

# Cas limite : p0 >= p1 -> doit stopper proprement
test_p0_sup_p1 <- tryCatch(
  {
    sample_size_phase2(p0 = 0.5, p1 = 0.3, method = "ahern")
    log_result("sample_size_phase2 - p0 >= p1 (doit echouer proprement)", "ECHEC", "aucune erreur levee")
  },
  error = function(e) {
    log_result("sample_size_phase2 - p0 >= p1 (doit echouer proprement)", "OK", conditionMessage(e))
  }
)

# Comparaison A'Hern vs Fleming : Fleming doit donner un n <= A'Hern
# (Fleming utilise l'approximation normale, generalement moins conservatrice)
if (!is.null(res_phase2_ahern) && !is.null(res_phase2_fleming)) {

  n_na <- sum(is.na(res_phase2_ahern$n_patient))
  if (n_na > 0) {
    log_result(
      "Coherence : n_patient(fleming) <= n_patient(ahern) (approx.)",
      "IGNORE",
      sprintf("%d combinaison(s) sans solution A'Hern dans nmax=100 (NA) -- augmentez nmax pour tester", n_na)
    )
  } else {
    ecart_ok <- all(res_phase2_fleming$n_patient <= res_phase2_ahern$n_patient + 2, na.rm = TRUE)
    log_result(
      "Coherence : n_patient(fleming) <= n_patient(ahern) (approx.)",
      if (ecart_ok) "OK" else "ECHEC",
      sprintf("ahern = %s / fleming = %s",
              paste(res_phase2_ahern$n_patient, collapse = ","),
              paste(res_phase2_fleming$n_patient, collapse = ","))
    )
  }
}


#==============================================================================
# 5ter. sample_size_precision() -- proportion / sensibilite / specificite
#==============================================================================

cat("\n===== sample_size_precision() =====\n")

res_precision_prop <- run_test("sample_size_precision - proportion seule", quote(
  sample_size_precision(
    p = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40),
    missing_prop = c(0.1, 0.2)
  )
))

res_precision_sens <- run_test("sample_size_precision - sensibilite seule", quote(
  sample_size_precision(
    sens = 0.85,
    prev = 0.10,
    precision = 0.05
  )
))

res_precision_spec <- run_test("sample_size_precision - specificite seule", quote(
  sample_size_precision(
    spec = 0.90,
    prev = 0.10,
    precision = 0.05
  )
))

res_precision_sens_spec <- run_test("sample_size_precision - sens + spec simultanes", quote(
  sample_size_precision(
    sens = 0.85,
    spec = 0.90,
    prev = 0.10,
    precision = 0.05
  )
))

# Cas d'erreur attendu : p et sens fournis simultanement
test_p_et_sens <- tryCatch(
  {
    sample_size_precision(p = 0.3, sens = 0.85, prev = 0.1)
    log_result("sample_size_precision - p + sens (doit echouer proprement)", "ECHEC", "aucune erreur levee")
  },
  error = function(e) {
    log_result("sample_size_precision - p + sens (doit echouer proprement)", "OK", conditionMessage(e))
  }
)

# Cas d'erreur attendu : aucun argument fourni
test_aucun_arg <- tryCatch(
  {
    sample_size_precision()
    log_result("sample_size_precision - aucun argument (doit echouer proprement)", "ECHEC", "aucune erreur levee")
  },
  error = function(e) {
    log_result("sample_size_precision - aucun argument (doit echouer proprement)", "OK", conditionMessage(e))
  }
)

# Cas d'erreur attendu : sens fourni sans prev
test_sens_sans_prev <- tryCatch(
  {
    sample_size_precision(sens = 0.85)
    log_result("sample_size_precision - sens sans prev (doit echouer proprement)", "ECHEC", "aucune erreur levee")
  },
  error = function(e) {
    log_result("sample_size_precision - sens sans prev (doit echouer proprement)", "OK", conditionMessage(e))
  }
)

# Verification attributs ssdesignr_type
if (!is.null(res_precision_prop)) {
  ok_type <- identical(attr(res_precision_prop, "ssdesignr_type"), "precision_prop")
  log_result("sample_size_precision - ssdesignr_type = precision_prop", if (ok_type) "OK" else "ECHEC")
}
if (!is.null(res_precision_sens_spec)) {
  ok_type <- identical(attr(res_precision_sens_spec, "ssdesignr_type"), "precision_sens_spec")
  log_result("sample_size_precision - ssdesignr_type = precision_sens_spec", if (ok_type) "OK" else "ECHEC")
}


#==============================================================================
# 6. ss_report() -- export Word pour CHAQUE resultat generé ci-dessus
#==============================================================================

cat("\n===== ss_report() : export Word =====\n")

resultats_a_exporter <- list(
  mean_sup_student   = res_mean_sup_student,
  mean_sup_welch     = res_mean_sup_welch,
  mean_sup_wilcoxon  = res_mean_sup_wilcoxon,
  mean_ni_student    = res_mean_ni_student,
  mean_ni_welch      = res_mean_ni_welch,
  prop_sup_khi2      = res_prop_sup_khi2,
  prop_sup_fisher    = res_prop_sup_fisher,
  prop_sup_mcnemar   = res_prop_sup_mcnemar,
  prop_ni            = res_prop_ni,
  cluster_crt        = res_cluster_crt,
  cluster_baseline   = res_cluster_baseline,
  cluster_sw         = res_cluster_sw,
  cluster_cv         = res_cluster_cv,
  phase2_ahern       = res_phase2_ahern,
  phase2_fleming     = res_phase2_fleming,
  precision_prop     = res_precision_prop,
  precision_sens     = res_precision_sens,
  precision_spec     = res_precision_spec,
  precision_sens_spec = res_precision_sens_spec
)

for (nom in names(resultats_a_exporter)) {

  res_i <- resultats_a_exporter[[nom]]

  if (is.null(res_i)) {
    log_result(paste("ss_report -", nom), "IGNORE", "resultat source absent (echec en amont)")
    next
  }

  fichier <- paste0("rapport_", nom, ".docx")

  chemin <- run_test(paste("ss_report -", nom), quote(
    ss_report(
      result          = res_i,
      file            = fichier,
      nom_etude       = paste("Etude test -", nom),
      investigateur   = "Pr A Martin",
      methodologiste  = "Dr B",
      biostatisticien = "C D",
      unit            = "cm",
      min_width       = 1.5,
      margin          = 1.5
    )
  ))

  if (!is.null(chemin)) check_docx(chemin)
}

# Test explicite avec col_widths + unit = "in", pour couvrir ce chemin de code
if (!is.null(res_prop_sup_khi2)) {
  chemin_cw <- run_test("ss_report - avec col_widths personnalisees (unit=in)", quote(
    ss_report(
      result    = res_prop_sup_khi2,
      file      = "rapport_prop_sup_colwidths.docx",
      nom_etude = "Test col_widths",
      unit      = "in",
      col_widths = c(p1 = 0.9, p2 = 0.9, puissance = 0.8, alpha = 0.7),
      min_width  = 0.5,
      margin     = 0.6
    )
  ))
  if (!is.null(chemin_cw)) check_docx(chemin_cw)
}

# Test explicite du rescale automatique (largeurs volontairement excessives)
if (!is.null(res_prop_ni)) {
  chemin_rescale <- run_test("ss_report - rescale automatique (largeurs excessives)", quote(
    ss_report(
      result    = res_prop_ni,
      file      = "rapport_prop_ni_rescale.docx",
      nom_etude = "Test rescale",
      col_widths = c(p1 = 8, p2 = 8, marge = 8, puissance = 8, alpha = 8),  # volontairement trop large
      unit       = "cm"
    )
  ))
  if (!is.null(chemin_rescale)) check_docx(chemin_rescale)
}

# Verification qu'un objet non-ssdesignr est bien rejete
test_result_invalide <- tryCatch(
  {
    ss_report(result = data.frame(x = 1), file = "ne_devrait_pas_exister.docx")
    log_result("ss_report - result invalide (doit echouer proprement)", "ECHEC", "aucune erreur levee")
  },
  error = function(e) {
    log_result("ss_report - result invalide (doit echouer proprement)", "OK", conditionMessage(e))
  }
)


#==============================================================================
# 7. Validation numerique -- comparaison a des formules de reference
#
# Objectif : au-dela de "la fonction ne plante pas", on verifie ici que les
# resultats sont numeriquement PROCHES d'une reference independante calculee
# avec le package pwr (formules classiques de Cohen), pour les cas les plus
# simples et les mieux documentes (test bilateral, groupes equilibres).
#
# On tolere un ecart de quelques sujets : rpact et pwr n'utilisent pas
# exactement la meme parametrisation/approximation, un ecart de 1-3 sujets
# est normal et ne signale pas un bug.
#==============================================================================

cat("\n===== Validation numerique (vs package pwr) =====\n")

if (!requireNamespace("pwr", quietly = TRUE)) {

  log_result("Validation numerique", "IGNORE", "package 'pwr' non installe (install.packages('pwr'))")

} else {

  check_close <- function(label, valeur_ssdesignr, valeur_reference, tolerance) {
    ecart <- abs(valeur_ssdesignr - valeur_reference)
    if (ecart <= tolerance) {
      log_result(label, "OK",
                 sprintf("ssdesignr = %s, reference = %s (ecart = %s)",
                         valeur_ssdesignr, round(valeur_reference, 1), round(ecart, 1)))
    } else {
      log_result(label, "ECHEC",
                 sprintf("ssdesignr = %s, reference = %s (ecart = %s > tolerance %s)",
                         valeur_ssdesignr, round(valeur_reference, 1), round(ecart, 1), tolerance))
    }
  }

  # --- Cas 1 : Student, bilateral, groupes equilibres --------------------
  # Reference : pwr::pwr.t.test() -- test t de Student classique.

  mu1 <- 55; mu2 <- 50; sd_commun <- 10; power_cible <- 0.80; alpha_cible <- 0.05

  d_cohen <- abs(mu1 - mu2) / sd_commun

  ref_pwr <- pwr::pwr.t.test(
    d           = d_cohen,
    power       = power_cible,
    sig.level   = alpha_cible,
    type        = "two.sample",
    alternative = "two.sided"
  )
  n_ref_par_groupe <- ceiling(ref_pwr$n)

  res_check1 <- ss_mean_sup(
    mu1 = mu1, mu2 = mu2, sd = sd_commun,
    power = power_cible, alpha = alpha_cible,
    kappa = 1, sided = 2, choice = "student"
  )

  check_close(
    "ss_mean_sup/student vs pwr::pwr.t.test (n par groupe)",
    res_check1$n1[1], n_ref_par_groupe, tolerance = 2
  )

  # --- Cas 2 : Khi-2, bilateral, groupes equilibres -----------------------
  # Reference : pwr::pwr.2p.test() -- test du Khi-2/z pour deux proportions.

  p1_ref <- 0.30; p2_ref <- 0.15

  h_cohen <- pwr::ES.h(p1_ref, p2_ref)

  ref_pwr_prop <- pwr::pwr.2p.test(
    h           = h_cohen,
    power       = power_cible,
    sig.level   = alpha_cible,
    alternative = "two.sided"
  )
  n_ref_par_groupe_prop <- ceiling(ref_pwr_prop$n)

  res_check2 <- ss_prop_sup(
    p1 = p1_ref, p2 = p2_ref,
    power = power_cible, alpha = alpha_cible,
    kappa = 1, sided = 2, choice = "khi2"
  )

  # Tolerance plus large ici : pwr utilise la transformation arcsinus,
  # rpact utilise l'approximation normale -- un ecart de quelques sujets
  # est attendu meme quand tout fonctionne correctement.
  check_close(
    "ss_prop_sup/khi2 vs pwr::pwr.2p.test (n par groupe)",
    res_check2$n1[1], n_ref_par_groupe_prop, tolerance = 5
  )

  # --- Cas 3 : coherence interne -- kappa=1 doit donner n1 == n2 ----------

  if (res_check1$n1[1] == res_check1$n2[1]) {
    log_result("Coherence interne : kappa=1 -> n1==n2 (mean_sup)", "OK")
  } else {
    log_result("Coherence interne : kappa=1 -> n1==n2 (mean_sup)", "ECHEC",
               sprintf("n1=%s, n2=%s", res_check1$n1[1], res_check1$n2[1]))
  }

  # --- Cas 4 : coherence interne -- n_total = n1 + n2 sur tous les resultats --

  verif_somme <- function(df, nom) {
    if (is.null(df)) return(invisible(NULL))
    if (!all(c("n1", "n2", "n_total") %in% names(df))) return(invisible(NULL))
    ok <- all(df$n1 + df$n2 == df$n_total)
    log_result(paste("Coherence n_total = n1+n2 :", nom), if (ok) "OK" else "ECHEC")
  }

  verif_somme(res_mean_sup_student, "mean_sup_student")
  verif_somme(res_mean_sup_welch,   "mean_sup_welch")
  verif_somme(res_mean_ni_student,  "mean_ni_student")
  verif_somme(res_prop_sup_khi2,    "prop_sup_khi2")
  verif_somme(res_prop_ni,          "prop_ni")

  # --- Cas 5 : coherence interne -- n_total_pdv >= n_total (jamais l'inverse) --

  verif_pdv <- function(df, nom) {
    if (is.null(df)) return(invisible(NULL))
    if (!all(c("n_total", "n_total_pdv") %in% names(df))) return(invisible(NULL))
    ok <- all(df$n_total_pdv >= df$n_total)
    log_result(paste("Coherence n_total_pdv >= n_total :", nom), if (ok) "OK" else "ECHEC")
  }

  verif_pdv(res_mean_sup_pdv,       "mean_sup (missing_prop vectoriel)")
  verif_pdv(res_prop_ni_desequilibre, "prop_ni (desequilibre + pdv)")
}


#==============================================================================
# 8. Recapitulatif
#==============================================================================

cat("\n\n===== RECAPITULATIF =====\n")
print(test_log, row.names = FALSE)

n_echecs <- sum(test_log$statut == "ECHEC")
n_ok     <- sum(test_log$statut %in% c("OK", "OK (avec warning)"))
n_ignore <- sum(test_log$statut == "IGNORE")

cat(sprintf(
  "\nTotal : %d test(s) -- %d OK, %d ECHEC, %d IGNORE\n",
  nrow(test_log), n_ok, n_echecs, n_ignore
))

if (n_echecs > 0) {
  cat("\n/!\\ Echecs detectes :\n")
  print(test_log[test_log$statut == "ECHEC", c("test", "message")], row.names = FALSE)
} else {
  cat("\nTous les tests critiques sont passes.\n")
}

cat("\nLes fichiers .docx generes sont dans :", out_dir, "\n")
