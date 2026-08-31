#==============================================================================
# SCRIPT DE VERIFICATION DU PACKAGE - trialdesign
#
# A executer sur la branche de developpement, AVANT fusion vers main/master.
#
# Ce script :
#   1. Regenere NAMESPACE et man/ a partir des roxygen (devtools::document())
#   2. Recherche les dependances potentiellement obsoletes (ex: clinfun)
#   3. Lance devtools::check() (verification complete du package)
#   4. Affiche un resume clair : pret a fusionner, ou pas encore
#
# Prerequis : etre a la racine du package (la ou se trouve DESCRIPTION),
# et etre sur la bonne branche Git (verifiez avec `git branch` dans un terminal
# AVANT de lancer ce script).
#==============================================================================

setwd("C:/Users/georgev/Documents/R Etudes/Randomisations/TrialDesign")  # racine du package

# --- Verification du repertoire ------------------------------------------

if (!file.exists("DESCRIPTION")) {
  stop(
    "Aucun fichier DESCRIPTION trouve dans le repertoire courant (",
    getwd(), "). ",
    "Ce script doit etre execute depuis la racine du package. ",
    "Utilisez setwd() pour vous positionner au bon endroit, ou ouvrez le ",
    "projet RStudio du package avant de lancer ce script."
  )
}

cat("Package detecte a :", getwd(), "\n\n")


# --- Etape 1 : documentation ------------------------------------------------

cat("===== ETAPE 1/3 : devtools::document() =====\n")
cat("(regeneration de NAMESPACE et man/ a partir des roxygen)\n\n")

doc_ok <- tryCatch(
  {
    devtools::document()
    TRUE
  },
  error = function(e) {
    cat("\n/!\\ Erreur lors de document() :\n", conditionMessage(e), "\n")
    FALSE
  }
)

if (!doc_ok) {
  stop("devtools::document() a echoue -- corrigez l'erreur ci-dessus avant de continuer.")
}

cat("\n-> document() termine sans erreur.\n\n")


# --- Etape 2 : audit rapide des dependances ---------------------------------

cat("===== ETAPE 2/3 : audit des dependances (DESCRIPTION vs code) =====\n\n")

# Packages declares dans DESCRIPTION (Imports + Suggests)
desc <- read.dcf("DESCRIPTION")

extraire_pkgs <- function(champ) {
  if (!champ %in% colnames(desc)) return(character(0))
  txt <- desc[1, champ]
  if (is.na(txt)) return(character(0))
  pkgs <- strsplit(txt, ",")[[1]]
  pkgs <- trimws(gsub("\\(.*\\)", "", pkgs))
  pkgs[pkgs != "R" & pkgs != ""]
}

pkgs_imports  <- extraire_pkgs("Imports")
pkgs_suggests <- extraire_pkgs("Suggests")
pkgs_declares <- c(pkgs_imports, pkgs_suggests)

cat("Packages declares dans DESCRIPTION :\n")
cat(" - Imports  :", paste(pkgs_imports, collapse = ", "), "\n")
cat(" - Suggests :", paste(pkgs_suggests, collapse = ", "), "\n\n")

# Packages reellement utilises dans le code (via :: ou @importFrom)
fichiers_r <- list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
code_complet <- paste(vapply(fichiers_r, function(f) paste(readLines(f, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")

pkgs_utilises <- character(0)
for (pkg in pkgs_imports) {
  motif <- paste0(pkg, "::")
  if (grepl(motif, code_complet, fixed = TRUE)) {
    pkgs_utilises <- c(pkgs_utilises, pkg)
  }
}

pkgs_non_utilises <- setdiff(pkgs_imports, pkgs_utilises)

if (length(pkgs_non_utilises) > 0) {
  cat("/!\\ Packages declares dans Imports mais jamais appeles via 'pkg::' dans R/ :\n")
  cat("   ", paste(pkgs_non_utilises, collapse = ", "), "\n")
  cat("   -> Verifiez s'ils sont encore necessaires (ex: clinfun, retire de ss_phase2()).\n")
  cat("   -> Si inutiles, retirez-les de la section Imports du DESCRIPTION.\n\n")
} else {
  cat("-> Tous les packages d'Imports semblent utilises quelque part dans R/.\n\n")
}

# Recherche inverse : packages utilises via :: mais absents de DESCRIPTION
motif_double_colon <- "([a-zA-Z][a-zA-Z0-9\\.]*)::"
matches <- regmatches(code_complet, gregexpr(motif_double_colon, code_complet))[[1]]
pkgs_appeles <- unique(gsub("::$", "", matches))
pkgs_appeles <- setdiff(pkgs_appeles, c("base", "stats", "utils", "methods", "grDevices", "graphics"))

pkgs_manquants <- setdiff(pkgs_appeles, pkgs_declares)

if (length(pkgs_manquants) > 0) {
  cat("/!\\ Packages appeles via 'pkg::' dans R/ mais absents de DESCRIPTION :\n")
  cat("   ", paste(pkgs_manquants, collapse = ", "), "\n")
  cat("   -> Ajoutez-les a la section Imports du DESCRIPTION (usethis::use_package('nom')).\n\n")
} else {
  cat("-> Tous les packages appeles via '::' sont bien declares dans DESCRIPTION.\n\n")
}


# --- Etape 3 : devtools::check() --------------------------------------------

cat("===== ETAPE 3/3 : devtools::check() =====\n")
cat("(verification complete du package -- peut prendre plusieurs minutes)\n\n")

check_result <- tryCatch(
  devtools::check(quiet = FALSE),
  error = function(e) {
    cat("\n/!\\ devtools::check() a leve une erreur :\n", conditionMessage(e), "\n")
    NULL
  }
)


# --- Resume final ------------------------------------------------------------

cat("\n\n===== RESUME =====\n\n")

if (is.null(check_result)) {

  cat("/!\\ Le check n'a pas pu se terminer normalement. Consultez les erreurs ci-dessus.\n")

} else {

  n_errors   <- length(check_result$errors)
  n_warnings <- length(check_result$warnings)
  n_notes    <- length(check_result$notes)

  cat(sprintf("Erreurs (errors)     : %d\n", n_errors))
  cat(sprintf("Avertissements (warn) : %d\n", n_warnings))
  cat(sprintf("Notes (notes)         : %d\n", n_notes))

  cat("\n")

  if (n_errors == 0 && n_warnings == 0) {
    cat("✔ Package pret a etre fusionne vers la branche principale.\n")
    if (n_notes > 0) {
      cat("  (", n_notes, "note(s) restante(s) -- generalement tolerables, mais jetez-y un oeil.)\n", sep = "")
    }
  } else {
    cat("✘ Corrigez les erreurs/avertissements ci-dessus AVANT de fusionner.\n")
    if (n_errors > 0) {
      cat("\nErreurs :\n")
      print(check_result$errors)
    }
    if (n_warnings > 0) {
      cat("\nAvertissements :\n")
      print(check_result$warnings)
    }
  }
}

cat("\nPensez egalement a relancer votre script de test global (test_ssdesignr.R)\n")
cat("si vous avez modifie du code depuis la derniere execution.\n")

