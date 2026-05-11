# Packages LaTeX requis pour la generation du PDF
.latex_pkgs <- c("lastpage", "tabularx", "array", "helvet",
                 "fontenc", "booktabs", "longtable", "multirow",
                 "float", "colortbl", "environ", "trimspaces",
                 "ulem", "makecell")

# Verifie et installe silencieusement les packages LaTeX manquants
.check_latex_deps <- function() {
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    warning("Le package 'tinytex' n'est pas installe. ",
            "Impossible de verifier les dependances LaTeX.")
    return(invisible(NULL))
  }
  installed <- tinytex::tl_pkgs()
  manquants <- .latex_pkgs[!.latex_pkgs %in% installed]
  if (length(manquants) > 0) {
    message("Installation des packages LaTeX manquants : ",
            paste(manquants, collapse = ", "), " ...")
    tinytex::tlmgr_install(manquants)
    message("Installation terminee.")
  }
  invisible(NULL)
}

.export_pdf <- function(df, nom_pdf, chemin,
                        page_garde = TRUE,
                        type_doc   = "randomisation",
                        col_widths = NULL) {

  # Verification et installation des dependances LaTeX
  .check_latex_deps()

  df_pdf <- .rename_pdf(df)

  # Construire l'appel kable selon col_widths
  if (!is.null(col_widths)) {
    if (length(col_widths) != ncol(df_pdf)) {
      stop("'col_widths' doit avoir autant d'elements que de colonnes dans le data.frame.")
    }
    # Convertir le vecteur en code R lisible ex: c("2cm","3cm")
    widths_str <- paste0('c("', paste(col_widths, collapse = '","'), '")')
    kable_code <- paste0(
      "kable(df_pdf, align = 'c', row.names = FALSE) %>%\n",
      "  kableExtra::column_spec(seq_len(ncol(df_pdf)), width = ", widths_str, ")"
    )
    lib_code <- "library(knitr)\nlibrary(kableExtra)"
  } else {
    kable_code <- "kable(df_pdf, align = 'c', row.names = FALSE)"
    lib_code   <- "library(knitr)"
  }

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
    "  - \\usepackage[scaled]{helvet}",
    "  - \\usepackage[T1]{fontenc}",
    "  - \\renewcommand{\\familydefault}{\\sfdefault}",
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
    lib_code,
    kable_code,
    "```"
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
