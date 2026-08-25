# TrialDesign

<!-- badges: start -->
<!-- badges: end -->

TrialDesign provides tools for the design of clinical trials, including
randomization and treatment allocation list generation and sample size
calculation.

The package supports block randomization with or without stratification,
with export formats compatible with Ennov and REDCap.

Sample size methods include superiority and non-inferiority trials with
continuous or binary endpoints, cluster randomized trials, precision-based
calculations, and single-arm phase II designs based on A'Hern's and
Fleming's methods.

## Installation

You can install the development version of trialdesign from [GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("BiostatUSMR/TrialDesign")
library(TrialDesign)
```

## Additional requirements

Some reporting features generate PDF documents and require a LaTeX
distribution.

TinyTeX can be installed directly from R:

``` r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Example

### Randomization and treatment allocation lists.


``` r
# Clinical trial definition using init_essai():
essai <- init_essai(
  nom_etude       = "Study Name",
  libelle_etude   = "Example clinical trial",
  id_etude        = "CHUBXYYYY/NN",
  investigateur   = "Investigator name",
  methodologiste  = "Methodologist name",
  biostatisticien = "Biostatistician name",
  indice_document = "02",
  circuit         = "ennov",
  k               = 2,
  block_sizes     = c(4, 6),
  nb_block        = c(10, 10),
  arm_label       = c("Treatment", "Placebo"),
  strat_vars      = list(
    sexe   = list(codes = c(1, 2), labels = c("Female", "Male")),
    centre = list(codes = c(1, 2), labels = c("Centre1", "Centre2"))
  )
)

# Randomization list
randomization  <- rand(
     essai,
     seed       = 42,
     statut     = "FICTIVE",
     version    = "V1",
     col_widths = NULL,
     chemin     = NULL
     )
head(randomization)

# Treatment allocation list
allocation <- corresp(
     essai,
     mini       = 1,
     maxi       = 50,
     seed       = 42,
     statut     = "FICTIVE",
     version    = "v01",
     boi_label  = NULL,
     col_widths = c("4cm", "4cm", "7cm"),
     chemin     = NULL
     )
head(allocation)
```
Both functions return the generated data frame and automatically export
the corresponding files.

### Sample size calculation

``` r
# Sample size for a superiority trial comparing two means
res <- ss_mean_sup(
  mu1    = 60,
  mu2    = 50,
  sd     = 10,
  power  = 0.80,
  alpha  = 0.05,
  choice = "student"
)
res

# Result subsequently adjusted for a cluster randomized design
res_cluster <- ss_cluster(
  n_ind  = res,
  schema = "crt",
  m      = 25,
  icc    = 0.05
)
res_cluster

# Generate a Word report from the sample size calculation
ss_report(
  result          = res_cluster,
  file            = "sample_size_report.docx",
  nom_etude       = "Study name",
  investigateur   = "Investigator name",
  methodologiste  = "Methodologist name",
  biostatisticien = "Biostatistician name"
)
```
