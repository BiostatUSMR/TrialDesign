#' Générer le code LaTeX de la page de garde
#'
#' Cette fonction récupère les informations de l'étude stockées dans l'environnement
#' interne du package (`.trialdesign_env`) pour générer le bloc LaTeX correspondant
#' à la page de garde du document.
#'
#' @param type Caractère. Type de document à générer : "randomisation" (par défaut)
#' pour la liste de randomisation, ou tout autre valeur pour la liste de correspondance boîtes/traitements.
#'
#' @return Une chaîne de caractères (string) contenant le code source LaTeX de la page de garde.
#'
#' @details La fonction nécessite que l'environnement `.trialdesign_env` soit
#' préalablement renseigné avec les variables : `nom_etude`, `id_etude`,
#' `libelle_etude`, `investigateur`, `biostatisticien`, `methodologiste`,
#' `indice_document` et `code_usmr`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Remplissage de l'environnement interne pour le test
#' .trialdesign_env$nom_etude       <- "VACCIN-COVID-2026"
#' .trialdesign_env$id_etude        <- "PHASE-III-8942"
#' .trialdesign_env$libelle_etude   <- "Évaluation de l'efficacité d'un candidat vaccin"
#' .trialdesign_env$investigateur   <- "Pr. Jean-Loup Durand"
#' .trialdesign_env$biostatisticien  <- "Mme Sarah Analyse"
#' .trialdesign_env$methodologiste   <- "Dr. Marc Protocol"
#' .trialdesign_env$indice_document  <- "A"
#' .trialdesign_env$code_usmr        <- "EN-USM-2026-001"
#'
#' # Génération et affichage du rendu
#' rando_tex <- .make_garde(type = "randomisation")
#' cat(rando_tex)
#' }
.make_garde <- function(type = "randomisation") {

  # Récupération des variables
  nom_etude     <- .trialdesign_env$nom_etude
  id_etude      <- .trialdesign_env$id_etude
  libelle_etude <- .trialdesign_env$libelle_etude
  investigateur <- .trialdesign_env$investigateur
  biostat       <- .trialdesign_env$biostatisticien
  methodo       <- .trialdesign_env$methodologiste
  indice_doc    <- .trialdesign_env$indice_document

  if (type == "randomisation") {
    titre_doc <- "LISTE DE RANDOMISATION"
    code_usmr <- .trialdesign_env$code_usmr
  } else {
    titre_doc <- "LISTE DE CORRESPONDANCE N\\textsuperscript{o} DE BO\\^{I}TE / TRAITEMENT"
    code_usmr <- "EN-USM-418"
  }

  logo <- system.file("LogoCHUBdx.jpg", package = "trialdesign")

  paste0(
    "\\begin{titlepage}
\\thispagestyle{empty}
\\sffamily

\\noindent
\\begin{tabular}{|m{0.28\\textwidth}|m{0.48\\textwidth}|m{0.18\\textwidth}|}
\\hline
\\centering \\vspace{2pt} \\includegraphics[width=0.25\\textwidth]{", gsub('\\\\','/', logo), "} \\vspace{2pt} &
\\centering \\textbf{Entité d'application : USMR} \\par \\textbf{Emetteur : USMR} &
\\centering \\textbf{", code_usmr, "} \\tabularnewline \\hline
& \\centering \\textbf{DOCUMENT D'ENREGISTREMENT} &
Ind : ", indice_doc, " \\par Page : 1 / \\pageref{LastPage} \\tabularnewline \\hline
\\multicolumn{3}{|c|}{\\rule{0pt}{3ex} \\large \\textbf{", titre_doc, "} \\rule[-1.5ex]{0pt}{0pt}} \\\\ \\hline
\\end{tabular}

\\vspace{2cm}

\\begin{center}
    {\\huge \\textbf{CONFIDENTIEL}}
\\end{center}

\\vspace{1.5cm}

\\noindent\\rule{\\textwidth}{0.8pt}
\\begin{center}
    \\vspace{0.3cm}
    {\\Large ", id_etude, "} \\\\[0.5cm]
    {\\Large ", libelle_etude, "} \\\\[0.5cm]
    {\\huge ", nom_etude, "}
    \\vspace{0.3cm}
\\end{center}
\\noindent\\rule{\\textwidth}{0.8pt}

\\vspace{1.5cm}

\\begin{flushleft}
    \\large
    \\textbf{Investigateur coordonnateur :} ", investigateur, " \\\\[0.3cm]
    \\textbf{Biostatisticien :} ", biostat, " \\\\[0.3cm]
    \\textbf{Méthodologiste :} ", methodo, "
\\end{flushleft}

\\end{titlepage}
\\setcounter{page}{2}
\\renewcommand{\\familydefault}{\\sfdefault}\\normalfont"
  )
}
