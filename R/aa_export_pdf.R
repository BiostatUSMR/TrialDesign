
############################################################
# FONCTION INTERNE .export_pdf


# Génération du PDF à partir d'un data.frame
.export_pdf <- function(df, nom_pdf, chemin) {

  df_pdf   <- .rename_pdf(df)
  rmd_temp <- tempfile(fileext = ".Rmd")

  writeLines(c(
    "---",
    "output:",
    "  pdf_document:",
    "    latex_engine: xelatex",
    "geometry: margin=2cm",
    "---",
    "",
    "```{r echo=FALSE, message=FALSE, warning=FALSE}",
    "library(knitr)",
    "kable(df_pdf, align = 'c', row.names = FALSE)",
    "```"
  ), rmd_temp)

  rmarkdown::render(
    input       = rmd_temp,
    output_file = nom_pdf,
    output_dir  = chemin,
    envir       = environment(),
    quiet       = TRUE
  )

  file.remove(rmd_temp)
}
