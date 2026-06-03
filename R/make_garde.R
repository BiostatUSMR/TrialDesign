###########################################################
# FONCTION INTERNE .make_garde()

.make_garde <- function(essai, type = "randomisation") {

  nom_etude     <- essai$nom_etude
  id_etude      <- essai$id_etude
  libelle_etude <- essai$libelle_etude
  investigateur <- essai$investigateur
  biostat       <- essai$biostatisticien
  methodo       <- essai$methodologiste
  indice_doc    <- essai$indice_document

  if (type == "randomisation") {
    titre_doc <- "LISTE DE RANDOMISATION"
    code_usmr <- essai$code_usmr
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
\\centering \\vspace{2pt} \\includegraphics[width=0.25\\textwidth]{", gsub('\\\\', '/', logo), "} \\vspace{2pt} &
\\centering \\textbf{Entit\u00e9 d'application : USMR} \\par \\textbf{Emetteur : USMR} &
\\centering \\textbf{", code_usmr, "} \\tabularnewline \\hline
& \\centering \\textbf{DOCUMENT D'ENREGISTREMENT} &
Ind : ", indice_doc, " \\par Page : 1 / \\pageref{enddocument} \\tabularnewline \\hline
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
    \\textbf{M\u00e9thodologiste :} ", methodo, "
\\end{flushleft}

\\end{titlepage}
\\setcounter{page}{2}
\\renewcommand{\\familydefault}{\\sfdefault}\\normalfont"
  )
}
