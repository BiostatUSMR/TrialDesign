###########################################################
# FONCTION INTERNE .export_pdf()

.export_pdf <- function(essai, df, nom_pdf, chemin,
                        page_garde = TRUE,
                        type_doc   = "randomisation",
                        version_doc = NULL,
                        col_widths = NULL) {

  df_pdf <- .rename_pdf(df)

  # Desactivation des packages auto kableExtra
  options(kableExtra.latex.load_packages = FALSE)

  # Construction kable
  if (!is.null(col_widths)) {
    if (length(col_widths) != ncol(df_pdf)) {
      stop("'col_widths' doit avoir autant d'elements que de colonnes dans le data.frame.")
    }
    widths_str <- paste0('c("', paste(col_widths, collapse = '","'), '")')
    kable_code <- paste0(
      "kable(df_pdf, ",
      "format = 'latex', ",
      "align = 'c', ",
      "row.names = FALSE, ",
      "booktabs = TRUE, ",
      "longtable = TRUE, ",
      "linesep = '', ",
      "escape = FALSE) %>%\n",
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
      "longtable = TRUE, ",
      "linesep = '', ",
      "escape = FALSE)"
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
    "  - \\usepackage{fancyhdr}",
    "  - \\usepackage{lastpage}",
    "  - \\usepackage{multirow}",
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

      "\\pagestyle{fancy}",
      "\\fancyhf{}",

      "\\renewcommand{\\headrulewidth}{0pt}",
      "\\renewcommand{\\footrulewidth}{0pt}",

      paste0("\\fancyhead[L]{\\small ", .escape_latex(essai$nom_etude),"}"),
      paste0("\\fancyhead[R]{\\small Version ", .escape_latex(version_doc), " du ", format(Sys.Date(), "%d/%m/%Y"), "}"),

      paste0("\\fancyfoot[L]{\\small ",
        if(type_doc == "randomisation") "Liste de randomisation" else "Liste de correspondance",
        " - CONFIDENTIEL - ", .escape_latex(essai$biostatisticien),"}"),

      "\\fancyfoot[R]{\\small Page \\thepage/\\pageref{LastPage}}",

      ""
    )
  }

  contenu <- c(
    contenu,
    "\\centering",
    "```{r echo=FALSE, message=FALSE, warning=FALSE}",
    lib_code,
    kable_code,
    "```",
    "\\label{enddocument}"
  )

  writeLines(contenu, rmd_temp)

  # Premiere passe : genere le PDF + garde les fichiers intermediaires (.tex, .aux)
  rmarkdown::render(
    input       = rmd_temp,
    output_file = nom_pdf,
    output_dir  = chemin,
    quiet       = TRUE,
    clean       = FALSE
  )

  # Seconde passe : relit le .aux genere, resout \pageref correctement
  rmarkdown::render(
    input       = rmd_temp,
    output_file = nom_pdf,
    output_dir  = chemin,
    quiet       = TRUE,
    clean       = TRUE
  )

  file.remove(rmd_temp)
}
