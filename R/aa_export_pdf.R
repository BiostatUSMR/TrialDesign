###########################################################
# FONCTION INTERNE .export_pdf()

.export_pdf <- function(essai, df, nom_pdf, chemin,
                        page_garde = TRUE,
                        type_doc   = "randomisation",
                        col_widths = NULL) {

  df_pdf <- .rename_pdf(df)

  # D\u00e9sactivation des packages auto kableExtra
  options(kableExtra.latex.load_packages = FALSE)

  # Construction kable
  if (!is.null(col_widths)) {
    if (length(col_widths) != ncol(df_pdf)) {
      stop("'col_widths' doit avoir autant d'\u00e9l\u00e9ments que de colonnes dans le data.frame.")
    }
    widths_str <- paste0('c("', paste(col_widths, collapse = '","'), '")')
    kable_code <- paste0(
      "kable(df_pdf, ",
      "format = 'latex', ",
      "align = 'c', ",
      "row.names = FALSE, ",
      "booktabs = TRUE, ",
      "longtable = TRUE) %>%\n",
      "kableExtra::column_spec(seq_len(ncol(df_pdf)), width = ",
      widths_str,
      ")"
    )
    lib_code <- "library(knitr)\nlibrary(kableExtra)"
  } else {
    kable_code <- paste0(
      "kable(df_pdf, ",
      "format = 'latex', ",
      "align = 'c', ",
      "row.names = FALSE, ",
      "booktabs = TRUE, ",
      "longtable = TRUE)"
    )
    lib_code <- "library(knitr)"
  }

  rmd_temp <- tempfile(fileext = ".Rmd")

  contenu <- c(
    "---",
    "output:",
    "  pdf_document:",
    "    latex_engine: xelatex",
    "header-includes:",
    "  - \\usepackage{graphicx}",
    "  - \\usepackage{array}",
    "  - \\usepackage{tabularx}",
    "  - \\usepackage{booktabs}",
    "  - \\usepackage{longtable}",
    "  - \\usepackage[T1]{fontenc}",
    "  - \\usepackage[scaled]{helvet}",
    "  - \\renewcommand{\\familydefault}{\\sfdefault}",
    "geometry: margin=2cm",
    "---",
    ""
  )

  if (page_garde) {
    contenu <- c(
      contenu,
      .make_garde(essai, type = type_doc),
      "\\newpage",
      ""
    )
  }

  contenu <- c(
    contenu,
    "```{r echo=FALSE, message=FALSE, warning=FALSE}",
    lib_code,
    kable_code,
    "```",
    "\\label{enddocument}"
  )

  writeLines(contenu, rmd_temp)

  rmarkdown::render(
    input       = rmd_temp,
    output_file = nom_pdf,
    output_dir  = chemin,
    quiet       = TRUE
  )

  file.remove(rmd_temp)
}
