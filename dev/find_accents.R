#==============================================================================
# REPERAGE DES CARACTERES NON-ASCII (accents) DANS LE PACKAGE
#
# A executer depuis la racine du package (la ou se trouve DESCRIPTION).
# Affiche, pour chaque fichier concerne, les lignes contenant des accents,
# avec le numero de ligne et le caractere en cause surligne.
#==============================================================================

fichiers_r <- list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

cat("Analyse de", length(fichiers_r), "fichier(s) dans R/...\n\n")

total_lignes_concernees <- 0

for (f in fichiers_r) {

  lignes <- readLines(f, warn = FALSE, encoding = "UTF-8")

  # Detecte les lignes contenant au moins un caractere hors plage ASCII (0-127)
  lignes_non_ascii <- which(vapply(
    lignes,
    function(l) any(utf8ToInt(enc2utf8(l)) > 127),
    logical(1)
  ))

  if (length(lignes_non_ascii) > 0) {
    cat("─────────────────────────────────────────────\n")
    cat("Fichier :", f, "\n")
    cat("─────────────────────────────────────────────\n")

    for (n in lignes_non_ascii) {
      ligne <- lignes[n]
      codes <- utf8ToInt(enc2utf8(ligne))
      car_speciaux <- unique(intToUtf8(codes[codes > 127], multiple = TRUE))

      cat(sprintf("  Ligne %4d : %s\n", n, ligne))
      cat(sprintf("             -> caractere(s) non-ASCII : %s\n",
                  paste(car_speciaux, collapse = " ")))
    }
    cat("\n")
    total_lignes_concernees <- total_lignes_concernees + length(lignes_non_ascii)
  }
}

cat("=====================================================\n")
cat("Total :", total_lignes_concernees, "ligne(s) avec caractere(s) non-ASCII.\n")
cat("=====================================================\n\n")

cat("Rappel des caracteres francais les plus frequents et leur equivalent ASCII :\n")
cat("  é è ê ë  -> e\n")
cat("  à â      -> a\n")
cat("  ù û      -> u\n")
cat("  î ï      -> i\n")
cat("  ô        -> o\n")
cat("  ç        -> c\n")
cat("  œ        -> oe\n")
cat("  ' (apostrophe typographique) -> ' (apostrophe simple)\n")

