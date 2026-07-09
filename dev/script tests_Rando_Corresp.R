#==============================================================================
# SCRIPT DE TEST - Fonctions de randomisation (package TrialDesign)
#
# Couvre : init_essai(), rand(), corresp()
# Sur le meme principe que test_ssdesignr.R (fonctions de calcul de NSN).
#
# Utilisation : source ce script depuis la racine de votre projet (ou avec
# devtools::load_all() déjà exécuté). Les fichiers generes vont dans un
# sous-dossier "test_outputs_rando/" du répertoire de travail courant.
# Necessite un environnement LaTeX fonctionnel (tinytex) pour generation des PDF.
#==============================================================================

setwd(here::here())

devtools::load_all()

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)

out_dir <- file.path(old_wd, paste0("test_outputs_rando_", format(Sys.Date(), "%Y%m%d")))
if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
dir.create(out_dir)
setwd(out_dir)

test_log <- data.frame(test = character(0), statut = character(0), message = character(0), stringsAsFactors = FALSE)

log_result <- function(test, statut, message = "") {
  test_log <<- rbind(test_log, data.frame(test = test, statut = statut, message = message))
  cat(sprintf("[%s] %s %s\n", statut, test, ifelse(message == "", "", paste0("- ", message))))
}

run_test <- function(label, expr) {
  tryCatch(
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
      val <- withCallingHandlers(eval(expr), warning = function(ww) invokeRestart("muffleWarning"))
      log_result(label, "OK (avec warning)", conditionMessage(w))
      val
    }
  )
}

check_file <- function(path, label) {
  if (file.exists(path) && file.size(path) > 0) {
    log_result(label, "OK")
  } else {
    log_result(label, "ECHEC", "fichier absent ou vide")
  }
}


#==============================================================================
# 1. init_essai() -- cas valides et cas d'erreur
#==============================================================================

cat("\n===== init_essai() =====\n")

essai_ennov_1strate <- run_test("init_essai - ennov 1 strate", quote(
  init_essai(
    nom_etude = "TEST_ENNOV_1STRATE", circuit = "ennov", k = 2,
    block_sizes = c(4, 6), nb_block = c(5, 5),
    arm_label = c("1 - Traitement", "2 - Placebo"),
    strat_vars = list(centre = list(codes = c(1, 2), labels = c("Centre 1", "Centre 2")))
  )
))

essai_ennov_2strates <- run_test("init_essai - ennov 2 strates", quote(
  init_essai(
    nom_etude = "TEST_ENNOV_2STRATES", circuit = "ennov", k = 2,
    block_sizes = c(4, 6), nb_block = c(5, 5),
    arm_label = c("1 - Traitement", "2 - Placebo"),
    strat_vars = list(
      sexe   = list(codes = c(1, 2), labels = c("Homme", "Femme")),
      centre = list(codes = c(1, 2, 3), labels = c("Centre 1", "Centre 2", "Centre 3")))
  )
))

essai_redcap_0strate <- run_test("init_essai - redcap 0 strate", quote(
  init_essai(
    nom_etude = "TEST_REDCAP_0STRATE", circuit = "redcap", k = 2,
    block_sizes = c(4, 6), nb_block = c(5, 5),
    arm_label = c("1 - Traitement", "2 - Placebo"),
  )
))

essai_redcap_1strate <- run_test("init_essai - redcap 1 strate", quote(
  init_essai(
    nom_etude = "TEST_REDCAP_1STRATE", circuit = "redcap", k = 2,
    block_sizes = c(4, 6), nb_block = c(5, 5),
    arm_label = c("1 - Traitement", "2 - Placebo"),
    strat_vars = list(centre = list(codes = c(1, 2), labels = c("Centre 1", "Centre 2")))
  )
))

essai_redcap_2strates <- run_test("init_essai - redcap 2 strates", quote(
  init_essai(
    nom_etude = "TEST_REDCAP_2STRATES", circuit = "redcap", k = 2,
    block_sizes = c(4, 6), nb_block = c(5, 5),
    arm_label = c("1 - Traitement", "2 - Placebo"),
    strat_vars = list(
      sexe   = list(codes = c(1, 2), labels = c("Homme", "Femme")),
      centre = list(codes = c(1, 2, 3), labels = c("Centre 1", "Centre 2", "Centre 3")))
  )
))


# --- Cas d'erreur attendus ---

t <- tryCatch({
  init_essai(nom_etude = "X", circuit = "invalide", k = 2, block_sizes = c(4), nb_block = c(5))
  log_result("init_essai - circuit invalide (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("init_essai - circuit invalide (doit echouer)", "OK", conditionMessage(e)))

t <- tryCatch({
  init_essai(
    nom_etude = "X", circuit = "ennov", k = 2,
    block_sizes = c(4), nb_block = c(10),
    arm_label = c("Traitement", "Placebo")
    # pas de strat_vars -> doit echouer
  )
  log_result("init_essai - ennov sans strat_vars (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("init_essai - ennov sans strat_vars (doit echouer)", "OK", conditionMessage(e)))

t <- tryCatch({
  init_essai(
    nom_etude = "X", circuit = "redcap", k = 2,
    block_sizes = c(4), nb_block = c(10),
    arm_label = c("Traitement", "Placebo")
    # pas de strat_vars -> doit fonctionner (optionnel pour redcap)
  )
  log_result("init_essai - redcap sans strat_vars (doit fonctionner)", "OK")
}, error = function(e) log_result("init_essai - redcap sans strat_vars (doit fonctionner)", "ECHEC", conditionMessage(e)))

t <- tryCatch({
  init_essai(nom_etude = "X", circuit = "ennov", k = 1, block_sizes = c(4), nb_block = c(5))
  log_result("init_essai - k < 2 (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("init_essai - k < 2 (doit echouer)", "OK", conditionMessage(e)))

t <- tryCatch({
  init_essai(nom_etude = "X", circuit = "ennov", k = 2, block_sizes = c(5), nb_block = c(10))
  log_result("init_essai - block_sizes non divisible par sum(ratio) (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("init_essai - block_sizes non divisible par sum(ratio) (doit echouer)", "OK", conditionMessage(e)))

t <- tryCatch({
  init_essai(nom_etude = "X", circuit = "ennov", k = 2, block_sizes = c(4, 6), nb_block = c(10))
  log_result("init_essai - block_sizes/nb_block longueurs differentes (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("init_essai - block_sizes/nb_block longueurs differentes (doit echouer)", "OK", conditionMessage(e)))

t <- tryCatch({
  init_essai(
    nom_etude = "X", circuit = "ennov", k = 2, block_sizes = c(4), nb_block = c(10),
    strat_vars = list(centre = list(codes = c(1, 1), labels = c("A", "B")))
  )
  log_result("init_essai - codes strat_vars dupliques (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("init_essai - codes strat_vars dupliques (doit echouer)", "OK", conditionMessage(e)))

t <- tryCatch({
  init_essai(
    nom_etude = "X", circuit = "ennov", k = 2, block_sizes = c(4), nb_block = c(10),
    strat_vars = list(centre = list(codes = c(1, 2), labels = c("A")))
  )
  log_result("init_essai - codes/labels longueurs differentes (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("init_essai - codes/labels longueurs differentes (doit echouer)", "OK", conditionMessage(e)))


#==============================================================================
# 2. rand() -- generation + coherence + fichiers
#==============================================================================

cat("\n===== rand() =====\n")

# Ennov 1 variable de stratification
df_rand_ennov_1strate <- run_test("rand - ennov (1 variable de stratification)", quote(
  rand(essai_ennov_1strate, seed = 42, statut = "FICTIVE", version = "v01", chemin = out_dir)
))

if (!is.null(df_rand_ennov_1strate)) {
  n_strates_simple <- length(essai_ennov_1strate$strat_vars$centre$codes)
  n_attendu <- sum(essai_ennov_1strate$block_sizes * essai_ennov_1strate$nb_block) * n_strates_simple
  ok_n <- nrow(df_rand_ennov_1strate) == n_attendu
  log_result("rand - coherence nrow == sum(block_sizes*nb_block)*n_strates", if (ok_n) "OK" else "ECHEC",
             sprintf("attendu=%d, obtenu=%d", n_attendu, nrow(df_rand_ennov_1strate)))

  ok_grp <- all(df_rand_ennov_1strate$rdgrp %in% essai_ennov_1strate$arm_code)
  log_result("rand - coherence rdgrp dans arm_code", if (ok_grp) "OK" else "ECHEC")

  fichiers_attendus <- list.files(out_dir, pattern = "TEST_ENNOV.*Liste de randomisation")
  log_result("rand - fichiers generes (ennov)", if (length(fichiers_attendus) >= 2) "OK" else "ECHEC",
             paste(fichiers_attendus, collapse = " | "))
}

# Ennov 2 variables de stratification
df_rand_ennov_2strates <- run_test("rand - ennov (2 variables de stratification)", quote(
  rand(essai_ennov_2strates, seed = 42, statut = "FICTIVE", version = "v01", chemin = out_dir)
))

if (!is.null(df_rand_ennov_2strates)) {
  n_par_strate <- sum(essai_ennov_2strates$block_sizes * essai_ennov_2strates$nb_block)
  n_strates    <- prod(sapply(essai_ennov_2strates$strat_vars, function(x) length(x$codes)))

  ok_n <- nrow(df_rand_ennov_2strates) == n_par_strate * n_strates
  log_result("rand - coherence nrow avec strates", if (ok_n) "OK" else "ECHEC",
             sprintf("attendu=%d, obtenu=%d", n_par_strate * n_strates, nrow(df_rand_ennov_2strates)))

  # rdnum doit redemarrer a 1 pour chaque strate
  rdnum_par_strate <- tapply(df_rand_ennov_2strates$rdnum, df_rand_ennov_2strates$rdstr, function(x) x[1])
  ok_restart <- all(rdnum_par_strate == 1)
  log_result("rand - rdnum redemarre a 1 par strate", if (ok_restart) "OK" else "ECHEC")
}

# Redcap 0 variable de stratification
df_rand_redcap_0strate <- run_test("rand - redcap (0 variable de stratification)", quote(
  rand(essai_redcap_0strate, seed = 7, statut = "FICTIVE", version = "v01", chemin = out_dir)
))

if (!is.null(df_rand_redcap_0strate)) {
  fichiers_csv <- list.files(out_dir, pattern = "TEST_REDCAP.*\\.csv$")
  log_result("rand - fichier csv genere (redcap)", if (length(fichiers_csv) >= 1) "OK" else "ECHEC")
}

# Redcap 1 variables de stratification
df_rand_redcap_1strate <- run_test("rand - redcap (1 variable de stratification)", quote(
  rand(essai_redcap_1strate, seed = 42, statut = "FICTIVE", version = "v01", chemin = out_dir)
))

if (!is.null(df_rand_redcap_1strate)) {
  n_strates_simple <- length(essai_redcap_1strate$strat_vars$centre$codes)
  n_attendu <- sum(essai_redcap_1strate$block_sizes * essai_redcap_1strate$nb_block) * n_strates_simple
  ok_n <- nrow(df_rand_redcap_1strate) == n_attendu
  log_result("rand - coherence nrow == sum(block_sizes*nb_block)*n_strates", if (ok_n) "OK" else "ECHEC",
             sprintf("attendu=%d, obtenu=%d", n_attendu, nrow(df_rand_redcap_1strate)))

  ok_grp <- all(df_rand_redcap_1strate$rdgrp %in% essai_redcap_1strate$arm_code)
  log_result("rand - coherence rdgrp dans arm_code", if (ok_grp) "OK" else "ECHEC")

  fichiers_attendus <- list.files(out_dir, pattern = "TEST_ENNOV.*Liste de randomisation")
  log_result("rand - fichiers generes (ennov)", if (length(fichiers_attendus) >= 2) "OK" else "ECHEC",
             paste(fichiers_attendus, collapse = " | "))
}

# Redcap 2 variables de stratification
df_rand_redcap_2strates <- run_test("rand - ennov (2 variables de stratification)", quote(
  rand(essai_redcap_2strates, seed = 42, statut = "FICTIVE", version = "v01", chemin = out_dir,
       col_widths  = c("3cm", "2cm", "2.5cm", "2cm", "2cm", "2cm", "2cm"))))

if (!is.null(df_rand_redcap_2strates)) {
  n_par_strate <- sum(essai_redcap_2strates$block_sizes * essai_redcap_2strates$nb_block)
  n_strates    <- prod(sapply(essai_redcap_2strates$strat_vars, function(x) length(x$codes)))

  ok_n <- nrow(df_rand_redcap_2strates) == n_par_strate * n_strates
  log_result("rand - coherence nrow avec strates", if (ok_n) "OK" else "ECHEC",
             sprintf("attendu=%d, obtenu=%d", n_par_strate * n_strates, nrow(df_rand_redcap_2strates)))

  # rdnum doit redemarrer a 1 pour chaque strate
  rdnum_par_strate <- tapply(df_rand_redcap_2strates$rdnum,
                             interaction(df_rand_redcap_2strates$rdstr1, df_rand_redcap_2strates$rdstr2, drop = TRUE),
                             function(x) x[1])
  ok_restart <- all(rdnum_par_strate == 1)
  log_result("rand - rdnum redemarre a 1 par strate", if (ok_restart) "OK" else "ECHEC")
}


# --- Cas d'erreur attendus ---

t <- tryCatch({
  rand(list(circuit = NULL), seed = 1, statut = "FICTIVE", version = "v01")
  log_result("rand - essai invalide (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("rand - essai invalide (doit echouer)", "OK", conditionMessage(e)))

t <- tryCatch({
  rand(essai_ennov_simple, seed = 1.5, statut = "FICTIVE", version = "v01", chemin = out_dir)
  log_result("rand - seed non entier (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("rand - seed non entier (doit echouer)", "OK", conditionMessage(e)))

t <- tryCatch({
  rand(essai_ennov_simple, seed = 1, statut = "INVALIDE", version = "v01", chemin = out_dir)
  log_result("rand - statut invalide (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("rand - statut invalide (doit echouer)", "OK", conditionMessage(e)))

t <- tryCatch({
  rand(essai_ennov_simple, seed = 1, statut = "FICTIVE", version = "v01", chemin = "chemin/inexistant/xyz")
  log_result("rand - chemin inexistant (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("rand - chemin inexistant (doit echouer)", "OK", conditionMessage(e)))


#==============================================================================
# 3. corresp() -- generation + coherence + fichiers (txt/csv + pdf + xlsx)
#==============================================================================

cat("\n===== corresp() =====\n")

# Ennov 1 variable de stratification
df_corresp_ennov_1strate <- run_test("corresp - ennov 1 variable de stratification", quote(
  corresp(essai_ennov_1strate, mini = 1, maxi = 20, seed = 42, statut = "FICTIVE", version = "v01", chemin = out_dir)))

if (!is.null(df_corresp_ennov_1strate)) {
  ok_n <- nrow(df_corresp_ennov_1strate) == 20
  log_result("corresp - coherence nrow == maxi-mini+1", if (ok_n) "OK" else "ECHEC",
             sprintf("attendu=20, obtenu=%d", nrow(df_corresp_ennov_1strate)))

  ok_unique <- !any(duplicated(df_corresp_ennov_1strate$rdboi))
  log_result("corresp - rdboi sans doublon", if (ok_unique) "OK" else "ECHEC")

  fichiers_attendus <- list.files(out_dir, pattern = "TEST_ENNOV_1STRATE.*Liste de correspondance")
  log_result("corresp - fichiers generes (ennov: txt+pdf+xlsx)",
             if (length(fichiers_attendus) >= 3) "OK" else "ECHEC",
             paste(fichiers_attendus, collapse = " | "))
}

# Ennov 2 variables de stratification
df_corresp_ennov_2strates <- run_test("corresp - ennov 2 variables de stratification", quote(
  corresp(essai_ennov_2strates, mini = 1, maxi = 20, seed = 42, statut = "FICTIVE", version = "v01", chemin = out_dir)))

if (!is.null(df_corresp_ennov_2strates)) {
  ok_n <- nrow(df_corresp_ennov_2strates) == 20
  log_result("corresp - coherence nrow == maxi-mini+1", if (ok_n) "OK" else "ECHEC",
             sprintf("attendu=20, obtenu=%d", nrow(df_corresp_ennov_2strates)))

  ok_unique <- !any(duplicated(df_corresp_ennov_2strates$rdboi))
  log_result("corresp - rdboi sans doublon", if (ok_unique) "OK" else "ECHEC")

  fichiers_attendus <- list.files(out_dir, pattern = "TEST_ENNOV_2STRATES.*Liste de correspondance")
  log_result("corresp - fichiers generes (ennov: txt+pdf+xlsx)",
             if (length(fichiers_attendus) >= 3) "OK" else "ECHEC",
             paste(fichiers_attendus, collapse = " | "))
}


# Redcap 0 variable de stratification
df_corresp_redcap_0strate <- run_test("corresp - redcap 0 variable de stratification avec boi_label personnalise", quote(
  corresp(essai_redcap_0strate, mini = 101, maxi = 120, seed = 7, statut = "FICTIVE", version = "v01", boi_label = "Flacon", chemin = out_dir)))

if (!is.null(df_corresp_redcap_0strate)) {
  ok_label <- all(grepl("^Flacon", df_corresp_redcap_0strate$rdboi_lib))
  log_result("corresp - boi_label personnalise applique", if (ok_label) "OK" else "ECHEC")

  fichiers_csv <- list.files(out_dir, pattern = "TEST_REDCAP_0STRATE.*\\.csv$")
  log_result("corresp - fichier csv genere (redcap)", if (length(fichiers_csv) >= 1) "OK" else "ECHEC")
}

# Redcap 1 variable de stratification
df_corresp_redcap_1strate <- run_test("corresp - redcap 1 variable de stratification avec boi_label personnalise", quote(
  corresp(essai_redcap_1strate, mini = 101, maxi = 120, seed = 7, statut = "FICTIVE", version = "v01", boi_label = "Flacon", chemin = out_dir)))

if (!is.null(df_corresp_redcap_1strate)) {
  ok_label <- all(grepl("^Flacon", df_corresp_redcap_1strate$rdboi_lib))
  log_result("corresp - boi_label personnalise applique", if (ok_label) "OK" else "ECHEC")

  fichiers_csv <- list.files(out_dir, pattern = "TEST_REDCAP_1STRATE.*\\.csv$")
  log_result("corresp - fichier csv genere (redcap)", if (length(fichiers_csv) >= 1) "OK" else "ECHEC")
}

# Redcap 2 variables de stratification
df_corresp_redcap_2strates <- run_test("corresp - redcap 2 variables de stratification avec boi_label personnalise", quote(
  corresp(essai_redcap_2strates, mini = 10001, maxi = 10021, seed = 7, statut = "FICTIVE", version = "v01", boi_label = "Flacon", chemin = out_dir)))

if (!is.null(df_corresp_redcap_2strates)) {
  ok_label <- all(grepl("^Flacon", df_corresp_redcap_2strates$rdboi_lib))
  log_result("corresp - boi_label personnalise applique", if (ok_label) "OK" else "ECHEC")

  fichiers_csv <- list.files(out_dir, pattern = "TEST_REDCAP_2STRATES.*\\.csv$")
  log_result("corresp - fichier csv genere (redcap)", if (length(fichiers_csv) >= 1) "OK" else "ECHEC")
}

# --- Cas d'erreur attendus ---

t <- tryCatch({
  corresp(essai_ennov_1strate, mini = 50, maxi = 10, seed = 1, statut = "FICTIVE", version = "v01", chemin = out_dir)
  log_result("corresp - maxi <= mini (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("corresp - maxi <= mini (doit echouer)", "OK", conditionMessage(e)))

t <- tryCatch({
  corresp(essai_ennov_1strate, mini = 1, maxi = 10, seed = 1, statut = "FICTIVE", version = "v01",
          boi_label = 123, chemin = out_dir)
  log_result("corresp - boi_label non caractere (doit echouer)", "ECHEC", "aucune erreur levee")
}, error = function(e) log_result("corresp - boi_label non caractere (doit echouer)", "OK", conditionMessage(e)))


#==============================================================================
# 4. Recapitulatif
#==============================================================================

cat("\n\n===== RECAPITULATIF =====\n")
print(test_log, row.names = FALSE)

n_echecs <- sum(test_log$statut == "ECHEC")
n_ok     <- sum(test_log$statut %in% c("OK", "OK (avec warning)"))

cat(sprintf("\nTotal : %d test(s) -- %d OK, %d ECHEC\n", nrow(test_log), n_ok, n_echecs))

if (n_echecs > 0) {
  cat("\n/!\\ Echecs detectes :\n")
  print(test_log[test_log$statut == "ECHEC", c("test", "message")], row.names = FALSE)
} else {
  cat("\nTous les tests sont passes.\n")
}

cat("\nLes fichiers generes sont dans :", out_dir, "\n")
