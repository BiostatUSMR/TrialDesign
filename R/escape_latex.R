###########################################################
# FONCTION INTERNE .escape_latex()
#
# Echappe les caracteres speciaux LaTeX dans un texte libre fourni par
# l'utilisateur ou present dans les donnees, pour un rendu PDF correct
# quel que soit le moteur LaTeX utilise (pdflatex/xelatex).
###########################################################

.escape_latex <- function(x) {
  if (is.null(x)) return(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)  # backslash EN PREMIER, sinon double-echappement
  x <- gsub("_", "\\\\_", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("#", "\\\\#", x)
  x <- gsub("\\$", "\\\\$", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("\u00b0", "\\\\textdegree{}", x)  # degre "n\u00b0" -> \textdegree{}
  x
}
