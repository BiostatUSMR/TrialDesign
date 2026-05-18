
# trialdesign

<!-- badges: start -->
<!-- badges: end -->

The goal of trialdesign is to provide functions to generate Randomization Lists and calculate sample sizes  for clinical trials 

## Installation

You can install the development version of trialdesign from [GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("BiostatUSMR/TrialDesign")
```

## Additional requirements

This package require a LaTeX distribution.

We recommend installing TinyTeX in R with:

``` r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(trialdesign)
# INITIALISATION DE L'ESSAI
init_essai(
  nom_etude       = "Study Name",
  libelle_etude   = "COMOVA",
  id_etude        = "CHUBXYYYY/NN",
  investigateur   = "Investigator name",
  methodologiste  = "Jean Dupont",
  biostatisticien = "George Frais",
  code_usmr       = "EN-USM-417",
  indice_document = "02",
  circuit         = "ennov",
  k               = 2,
  block_sizes     = c(4, 6),
  nb_block        = c(10, 10),
  arm_label       = c("Traitement", "Placebo"),
  strat_vars      = list(
    sexe   = list(codes = c(1, 2), labels = c("Femme", "Homme")),
    centre = list(codes = c(1, 2), labels = c("Centre1", "Centre2"))
  )
)


# LISTE DE RANDOMISATION
rand(seed = 42,
     statut = "FICTIVE",
     version = "V1",
     col_widths = NULL,
     chemin     = NULL )


# LISTE DE CORRESPODANCE
corresp(mini = 1,
        maxi = 50,
        seed = 42,
        statut = "FICTIVE",
        version = "v01",
        boi_label = NULL,
        col_widths = c("4cm", "4cm", "7cm"),
        chemin     = NULL)

```

