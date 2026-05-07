.export_pdf <- function(df, nom_pdf, chemin, page_garde = TRUE, type_doc = "randomisation") {

  df_pdf   <- .rename_pdf(df)
  rmd_temp <- tempfile(fileext = ".Rmd")

  contenu <- c(
    "---",
    "output:",
    "  pdf_document:",
    "    latex_engine: xelatex",
    "header-includes:",
    "  - \\usepackage{lastpage}",
    "  - \\usepackage{tabularx}",
    "  - \\usepackage{array}",
    "  - \\usepackage[scaled]{helvet} % Police proche de Arial",
    "  - \\usepackage[T1]{fontenc}",
    "  - \\renewcommand{\\familydefault}{\\sfdefault} % Force le Sans Serif (Arial) partout",
    "geometry: margin=2cm",
    "---",
    ""
  )

  if (page_garde) {
    contenu <- c(contenu, .make_garde(type = type_doc), "\\newpage", "")
  }

  contenu <- c(
    contenu,
    "```{r echo=FALSE, message=FALSE, warning=FALSE}",
    "library(knitr)",
    "kable(df_pdf, align = 'c', row.names = FALSE)",
    "```"
  )

  writeLines(contenu, rmd_temp)
  rmarkdown::render(input = rmd_temp, output_file = nom_pdf, output_dir = chemin, quiet = TRUE)
}
