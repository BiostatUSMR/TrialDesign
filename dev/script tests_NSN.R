#==============================================================================
# SCRIPT DE TEST - Fonctions de calcul NSN (Package TrialDesign)
#
# Objectif :
#   - Vérifier que chaque fonction de calcul tourne sans erreur.
#   - Tester les différentes valeurs de `choice`.
#   - Tester certains cas vectorisés.
#   - Vérifier certains comportements d'erreur attendus.
#   - Tester ss_cluster() sur les résultats compatibles.
#   - Tester ss_phase2().
#   - Tester ss_precision().
#   - Vérifier que ss_report() génère correctement un fichier HTML.
#
# Utilisation :
#   Lancer ce script depuis la racine du projet.
#
# Les fichiers HTML sont écrits dans un sous-dossier :
#   test_outputs_YYYYMMDD
#==============================================================================


#==============================================================================
# 0. INITIALISATION
#==============================================================================

setwd(here::here())

devtools::load_all()

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)


#------------------------------------------------------------------------------
# Dossier de sortie
#------------------------------------------------------------------------------

out_dir <- file.path(
  old_wd,
  paste0(
    "test_outputs_",
    format(Sys.Date(), "%Y%m%d")
  )
)

if (dir.exists(out_dir)) {
  unlink(out_dir, recursive = TRUE)
}

dir.create(out_dir)

setwd(out_dir)


#------------------------------------------------------------------------------
# Journal des tests
#------------------------------------------------------------------------------

test_log <- data.frame(
  test = character(0),
  statut = character(0),
  message = character(0),
  stringsAsFactors = FALSE
)


log_result <- function(test, statut, message = "") {

  test_log <<- rbind(
    test_log,
    data.frame(
      test = test,
      statut = statut,
      message = message,
      stringsAsFactors = FALSE
    )
  )

  cat(
    sprintf(
      "[%s] %s%s\n",
      statut,
      test,
      ifelse(
        message == "",
        "",
        paste0(" - ", message)
      )
    )
  )
}


#------------------------------------------------------------------------------
# Fonction générique de test
#------------------------------------------------------------------------------

run_test <- function(label, expr) {

  warnings <- character(0)

  res <- tryCatch(

    withCallingHandlers(

      eval(expr),

      warning = function(w) {

        warnings <<- c(
          warnings,
          conditionMessage(w)
        )

        invokeRestart("muffleWarning")
      }

    ),

    error = function(e) {

      log_result(
        label,
        "ECHEC",
        conditionMessage(e)
      )

      return(NULL)
    }

  )

  if (is.null(res)) {

    return(NULL)

  }

  if (length(warnings) > 0) {

    log_result(
      label,
      "OK (avec warning)",
      paste(
        unique(warnings),
        collapse = " | "
      )
    )

  } else {

    log_result(
      label,
      "OK"
    )

  }

  res
}


#------------------------------------------------------------------------------
# Vérification d'un fichier généré
#------------------------------------------------------------------------------

check_file <- function(path) {

  if (is.null(path)) {

    log_result(
      "Fichier généré",
      "ECHEC",
      "chemin NULL"
    )

    return(FALSE)
  }

  if (
    file.exists(path) &&
    file.size(path) > 0
  ) {

    log_result(
      paste(
        "Fichier généré :",
        basename(path)
      ),
      "OK"
    )

    return(TRUE)

  } else {

    log_result(
      paste(
        "Fichier généré :",
        basename(path)
      ),
      "ECHEC",
      "fichier absent ou vide"
    )

    return(FALSE)
  }
}


#------------------------------------------------------------------------------
# Test ss_report()
#------------------------------------------------------------------------------

test_report <- function(
    result,
    label
) {

  if (is.null(result)) {

    log_result(
      paste(
        "ss_report -",
        label
      ),
      "ECHEC",
      "résultat source NULL"
    )

    return(NULL)
  }

  path <- run_test(

    paste(
      "ss_report -",
      label
    ),

    quote(

      ss_report(

        result = RESULT_PLACEHOLDER,

        file = FILE_PLACEHOLDER,

        nom_etude = "Test clinical trial",

        investigateur = "Investigator name",

        methodologiste = "Methodologist name",

        biostatisticien = "Biostatistician name"

      )

    )

  )

  path
}


#==============================================================================
# 1. ss_mean_sup()
#==============================================================================

cat("\n")
cat("============================================================\n")
cat("ss_mean_sup()\n")
cat("============================================================\n")


#------------------------------------------------------------------------------
# Student
#------------------------------------------------------------------------------

res_mean_sup_student <- run_test(

  "ss_mean_sup - student",

  quote(

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

  )

)


#------------------------------------------------------------------------------
# Welch déséquilibré
#------------------------------------------------------------------------------

res_mean_sup_welch <- run_test(

  "ss_mean_sup - welch (desequilibre)",

  quote(

    ss_mean_sup(

      mu1    = c(55, 60),
      mu2    = 50,

      sd1    = 10,
      sd2    = 15,

      power  = 0.80,

      kappa  = 1 / 2,

      choice = "welch"

    )

  )

)


#------------------------------------------------------------------------------
# Wilcoxon
#------------------------------------------------------------------------------

res_mean_sup_wilcoxon <- run_test(

  "ss_mean_sup - wilcoxon",

  quote(

    ss_mean_sup(

      mu1    = 55,
      mu2    = 50,

      sd1    = 10,
      sd2    = 10,

      power  = 0.80,

      nsim   = 2000,

      seed   = 123,

      choice = "wilcoxon"

    )

  )

)


#------------------------------------------------------------------------------
# missing_prop vectoriel
#------------------------------------------------------------------------------

res_mean_sup_pdv <- run_test(

  "ss_mean_sup - missing_prop vectoriel",

  quote(

    ss_mean_sup(

      mu1          = 55,
      mu2          = 50,

      sd           = 10,

      missing_prop = c(
        0,
        0.10,
        0.20
      ),

      choice = "student"

    )

  )

)


#------------------------------------------------------------------------------
# Cas limite : mu1 == mu2
#------------------------------------------------------------------------------

res_mean_sup_egal <- run_test(

  "ss_mean_sup - mu1 == mu2",

  quote(

    ss_mean_sup(

      mu1 = 50,
      mu2 = 50,

      sd = 10,

      choice = "student"

    )

  )

)


if (!is.null(res_mean_sup_egal)) {

  if (nrow(res_mean_sup_egal) == 0) {

    log_result(
      "ss_mean_sup - mu1 == mu2 -> 0 ligne",
      "OK",
      "comportement attendu"
    )

  }

}


#==============================================================================
# 2. ss_mean_ni()
#==============================================================================

cat("\n")
cat("============================================================\n")
cat("ss_mean_ni()\n")
cat("============================================================\n")


#------------------------------------------------------------------------------
# Student
#------------------------------------------------------------------------------

res_mean_ni_student <- run_test(

  "ss_mean_ni - student",

  quote(

    ss_mean_ni(

      mu1    = 10,
      mu2    = 10,

      sd     = 3,

      marge  = c(
        0.5,
        1.5
      ),

      power  = c(
        0.80,
        0.90
      ),

      alpha  = 0.025,

      choice = "student"

    )

  )

)


#------------------------------------------------------------------------------
# Welch déséquilibré
#------------------------------------------------------------------------------

res_mean_ni_welch <- run_test(

  "ss_mean_ni - welch (desequilibre)",

  quote(

    ss_mean_ni(

      mu1    = 10,
      mu2    = 10,

      sd1    = 3,
      sd2    = 4,

      marge  = 1,

      kappa  = 2,

      choice = "welch"

    )

  )

)


#==============================================================================
# 3. ss_prop_sup()
#==============================================================================

cat("\n")
cat("============================================================\n")
cat("ss_prop_sup()\n")
cat("============================================================\n")


#------------------------------------------------------------------------------
# Khi-2
#------------------------------------------------------------------------------

res_prop_sup_khi2 <- run_test(

  "ss_prop_sup - khi2",

  quote(

    ss_prop_sup(

      p1 = c(
        0.20,
        0.30
      ),

      p2 = 0.70,

      power = c(
        0.80,
        0.90
      ),

      missing_prop = 0.05,

      kappa = 1,

      choice = "khi2",

      sided = 2

    )

  )

)


#------------------------------------------------------------------------------
# Fisher
#------------------------------------------------------------------------------

res_prop_sup_fisher <- run_test(

  "ss_prop_sup - fisher",

  quote(

    ss_prop_sup(

      p1 = 0.20,
      p2 = 0.70,

      power = 0.80,

      kappa = 0.5,

      choice = "fisher",

      sided = 2

    )

  )

)


#------------------------------------------------------------------------------
# McNemar
#------------------------------------------------------------------------------

res_prop_sup_mcnemar <- run_test(

  "ss_prop_sup - mcnemar",

  quote(

    ss_prop_sup(

      p01 = 0.10,
      p10 = 0.20,

      power = 0.20,

      choice = "mcnemar"

    )

  )

)


#------------------------------------------------------------------------------
# Fisher avec p1 < p2
#------------------------------------------------------------------------------

res_prop_sup_fisher_inv <- run_test(

  "ss_prop_sup - fisher (p1 < p2, sided = 1)",

  quote(

    ss_prop_sup(

      p1 = 0.20,
      p2 = 0.50,

      power = 0.80,

      choice = "fisher",

      sided = 1

    )

  )

)


#------------------------------------------------------------------------------
# p01 négatif : erreur attendue
#------------------------------------------------------------------------------

tryCatch(

  {

    ss_prop_sup(

      p01 = -0.10,
      p10 = 0.20,

      choice = "mcnemar"

    )

    log_result(

      "ss_prop_sup - p01 negatif",

      "ECHEC",

      "aucune erreur levée"

    )

  },

  error = function(e) {

    log_result(

      "ss_prop_sup - p01 negatif",

      "OK",

      conditionMessage(e)

    )

  }

)


#==============================================================================
# 4. ss_prop_ni()
#==============================================================================

cat("\n")
cat("============================================================\n")
cat("ss_prop_ni()\n")
cat("============================================================\n")


#------------------------------------------------------------------------------
# Khi-2 équilibré
#------------------------------------------------------------------------------

res_prop_ni <- run_test(

  "ss_prop_ni - khi2",

  quote(

    ss_prop_ni(

      p1 = c(
        0.20,
        0.30
      ),

      p2 = 0.70,

      marge = c(
        0.01,
        0.15,
        0.30
      ),

      power = c(
        0.80,
        0.90
      ),

      alpha = 0.025

    )

  )

)


#------------------------------------------------------------------------------
# Déséquilibré + données manquantes
#------------------------------------------------------------------------------

res_prop_ni_desequilibre <- run_test(

  "ss_prop_ni - khi2 desequilibre + pdv",

  quote(

    ss_prop_ni(

      p1 = 0.20,

      p2 = 0.70,

      marge = 0.15,

      kappa = 2,

      missing_prop = c(
        0.05,
        0.10
      )

    )

  )

)


#==============================================================================
# 5. ss_cluster()
#==============================================================================

cat("\n")
cat("============================================================\n")
cat("ss_cluster()\n")
cat("============================================================\n")


#------------------------------------------------------------------------------
# CRT
#------------------------------------------------------------------------------

res_cluster_crt <- run_test(

  "ss_cluster - crt",

  quote(

    ss_cluster(

      n_ind = res_mean_ni_student,

      schema = "crt",

      m = c(
        20,
        30,
        50
      ),

      icc = c(
        0.01,
        0.05,
        0.10
      )

    )

  )

)


#------------------------------------------------------------------------------
# Baseline
#------------------------------------------------------------------------------

res_cluster_baseline <- run_test(

  "ss_cluster - baseline",

  quote(

    ss_cluster(

      n_ind = res_mean_sup_student,

      schema = "baseline",

      m = 25,

      icc = 0.05

    )

  )

)


#------------------------------------------------------------------------------
# Stepped wedge
#------------------------------------------------------------------------------

res_cluster_sw <- run_test(

  "ss_cluster - stepped-wedge",

  quote(

    ss_cluster(

      n_ind = res_prop_sup_khi2,

      schema = "sw",

      m = c(
        20,
        30
      ),

      icc = c(
        0.05,
        0.10
      ),

      k_steps = c(
        3,
        5
      )

    )

  )

)


#------------------------------------------------------------------------------
# CRT avec coefficient de variation
#------------------------------------------------------------------------------

res_cluster_cv <- run_test(

  "ss_cluster - crt avec cv > 0",

  quote(

    ss_cluster(

      n_ind = res_prop_ni,

      schema = "crt",

      m = 25,

      icc = 0.05,

      cv = 0.30

    )

  )

)


#------------------------------------------------------------------------------
# n_ind invalide : erreur attendue
#------------------------------------------------------------------------------

tryCatch(

  {

    ss_cluster(

      n_ind = data.frame(
        x = 1
      ),

      schema = "crt",

      m = 25,

      icc = 0.05

    )

    log_result(

      "ss_cluster - n_ind invalide",

      "ECHEC",

      "aucune erreur levée"

    )

  },

  error = function(e) {

    log_result(

      "ss_cluster - n_ind invalide",

      "OK",

      conditionMessage(e)

    )

  }

)


#------------------------------------------------------------------------------
# Vérification des attributs
#------------------------------------------------------------------------------

if (!is.null(res_cluster_crt)) {

  a_type <- attr(
    res_cluster_crt,
    "ssdesignr_type"
  )

  a_cluster <- attr(
    res_cluster_crt,
    "ssdesignr_cluster"
  )


  if (
    identical(
      a_type,
      "mean_ni"
    ) &&
    isTRUE(a_cluster)
  ) {

    log_result(
      "ss_cluster - attributs ssdesignr_type/cluster",
      "OK"
    )

  } else {

    log_result(

      "ss_cluster - attributs ssdesignr_type/cluster",

      "ECHEC",

      paste(
        "type =",
        a_type,
        "/ cluster =",
        a_cluster
      )

    )

  }

}


#------------------------------------------------------------------------------
# Vérification des labels
#------------------------------------------------------------------------------

if (!is.null(res_cluster_crt)) {

  labs <- labelled::var_label(
    res_cluster_crt
  )

  n_labs_manquants <- sum(

    vapply(
      labs,
      is.null,
      logical(1)
    )

  )


  if (n_labs_manquants == 0) {

    log_result(
      "ss_cluster - labels sur toutes les colonnes",
      "OK"
    )

  } else {

    log_result(

      "ss_cluster - labels sur toutes les colonnes",

      "ECHEC",

      paste(
        n_labs_manquants,
        "colonne(s) sans label"
      )

    )

  }

}


#==============================================================================
# 6. ss_phase2()
#==============================================================================

cat("\n")
cat("============================================================\n")
cat("ss_phase2()\n")
cat("============================================================\n")


#------------------------------------------------------------------------------
# A'Hern
#------------------------------------------------------------------------------

res_phase2_ahern <- run_test(

  "ss_phase2 - ahern",

  quote(

    ss_phase2(

      p0 = c(
        0.10,
        0.20
      ),

      p1 = c(
        0.30,
        0.40
      ),

      alpha = 0.05,

      power = 0.80,

      method = "ahern",

      missing_prop = 0.10,

      nmax = 100

    )

  )

)


#------------------------------------------------------------------------------
# Fleming
#------------------------------------------------------------------------------

res_phase2_fleming <- run_test(

  "ss_phase2 - fleming",

  quote(

    ss_phase2(

      p0 = 0.20,

      p1 = 0.40,

      alpha = 0.05,

      power = 0.80,

      method = "fleming",

      missing_prop = 0.10

    )

  )

)


#==============================================================================
# 7. ss_precision()
#==============================================================================

cat("\n")
cat("============================================================\n")
cat("ss_precision()\n")
cat("============================================================\n")


#------------------------------------------------------------------------------
# Proportion
#------------------------------------------------------------------------------

res_precision_prop <- run_test(

  "ss_precision - proportion",

  quote(

    ss_precision(

      p = c(
        0.10,
        0.20,
        0.30
      ),

      precision = 0.05,

      conf.level = 0.95,

      missing_prop = 0.10

    )

  )

)


#------------------------------------------------------------------------------
# Sensibilité
#------------------------------------------------------------------------------

res_precision_sens <- run_test(

  "ss_precision - sensibilite",

  quote(

    ss_precision(

      sens = 0.80,

      prev = 0.30,

      precision = 0.05,

      conf.level = 0.95,

      missing_prop = 0.10

    )

  )

)


#------------------------------------------------------------------------------
# Spécificité
#------------------------------------------------------------------------------

res_precision_spec <- run_test(

  "ss_precision - specificite",

  quote(

    ss_precision(

      spec = 0.90,

      prev = 0.30,

      precision = 0.05,

      conf.level = 0.95,

      missing_prop = 0.10

    )

  )

)


#------------------------------------------------------------------------------
# Sensibilité + spécificité
#------------------------------------------------------------------------------

res_precision_sens_spec <- run_test(

  "ss_precision - sensibilite + specificite",

  quote(

    ss_precision(

      sens = 0.80,

      spec = 0.90,

      prev = 0.30,

      precision = 0.05,

      conf.level = 0.95,

      missing_prop = 0.10

    )

  )

)


#==============================================================================
# 8. TESTS ss_report() - EXPORT HTML
#==============================================================================

cat("\n")
cat("============================================================\n")
cat("ss_report() - HTML\n")
cat("============================================================\n")


#------------------------------------------------------------------------------
# Fonction simplifiée pour générer et vérifier un rapport
#------------------------------------------------------------------------------

generate_report <- function(
    result,
    label
) {

  if (is.null(result)) {

    log_result(

      paste(
        "ss_report -",
        label
      ),

      "ECHEC",

      "résultat source NULL"

    )

    return(NULL)

  }


  safe_label <- gsub(
    "[^A-Za-z0-9]",
    "_",
    label
  )


  path <- run_test(

    paste(
      "ss_report -",
      label
    ),

    substitute(

      ss_report(

        result = RESULT,

        file = FILE,

        nom_etude = "Test clinical trial",

        investigateur = "Investigator name",

        methodologiste = "Methodologist name",

        biostatisticien = "Biostatistician name"

      ),

      list(

        RESULT = result,

        FILE = paste0(
          "report_",
          safe_label,
          ".html"
        )

      )

    )

  )


  if (!is.null(path)) {

    check_file(
      path
    )

  }


  invisible(path)

}


#------------------------------------------------------------------------------
# Rapports sur résultats individuels
#------------------------------------------------------------------------------

generate_report(
  res_mean_sup_student,
  "mean_sup_student"
)

generate_report(
  res_mean_sup_welch,
  "mean_sup_welch"
)

generate_report(
  res_mean_ni_student,
  "mean_ni_student"
)

generate_report(
  res_mean_ni_welch,
  "mean_ni_welch"
)

generate_report(
  res_prop_sup_khi2,
  "prop_sup_khi2"
)

generate_report(
  res_prop_sup_fisher,
  "prop_sup_fisher"
)

generate_report(
  res_prop_sup_mcnemar,
  "prop_sup_mcnemar"
)

generate_report(
  res_prop_ni,
  "prop_ni"
)

generate_report(
  res_phase2_ahern,
  "phase2_ahern"
)

generate_report(
  res_phase2_fleming,
  "phase2_fleming"
)

generate_report(
  res_precision_prop,
  "precision_prop"
)

generate_report(
  res_precision_sens,
  "precision_sens"
)

generate_report(
  res_precision_spec,
  "precision_spec"
)

generate_report(
  res_precision_sens_spec,
  "precision_sens_spec"
)


#------------------------------------------------------------------------------
# Rapports sur résultats cluster
#------------------------------------------------------------------------------

generate_report(
  res_cluster_crt,
  "cluster_crt"
)

generate_report(
  res_cluster_baseline,
  "cluster_baseline"
)

generate_report(
  res_cluster_sw,
  "cluster_sw"
)

generate_report(
  res_cluster_cv,
  "cluster_cv"
)


#==============================================================================
# 9. RÉSUMÉ FINAL
#==============================================================================

cat("\n")
cat("============================================================\n")
cat("RÉSUMÉ DES TESTS\n")
cat("============================================================\n")


print(
  test_log,
  row.names = FALSE
)


cat("\n")


n_ok <- sum(
  grepl(
    "^OK",
    test_log$statut
  )
)

n_echec <- sum(
  test_log$statut == "ECHEC"
)


cat(
  "Tests réussis :",
  n_ok,
  "\n"
)

cat(
  "Tests en échec :",
  n_echec,
  "\n"
)

cat(
  "Dossier de sortie :",
  out_dir,
  "\n"
)


#------------------------------------------------------------------------------
# Arrêt avec erreur si au moins un test a échoué
#------------------------------------------------------------------------------

if (n_echec > 0) {

  stop(
    paste(
      "Le script de test a détecté",
      n_echec,
      "échec(s)."
    )
  )

} else {

  cat("\n")
  cat("============================================================\n")
  cat("TOUS LES TESTS SONT PASSÉS AVEC SUCCÈS\n")
  cat("============================================================\n")

}
