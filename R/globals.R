#==============================================================================
# GLOBALS
#
# Ce fichier declare les noms de colonnes utilises dans des expressions
# dplyr::mutate()/purrr::pmap() a travers le package. R CMD check ne peut
# pas savoir que ces noms sont des colonnes de data.frame (evaluees dans un
# contexte de tidy evaluation) et non des variables globales -- cet appel
# supprime les notes "no visible binding for global variable" qui en
# resultent, sans changer le comportement du code.
#==============================================================================

utils::globalVariables(c(
  "tmp", "n1", "n2", "n1_pdv", "n2_pdv", "n_total", "n_total_pdv",
  "test", "puissance", "deff", "m_eff", "k_par_bras", "n_cluster_pdv",
  "n_patient", "n_succes", "n_ajuste", "methode",
  "n_prop", "n_prop_ajuste", "n_sens", "n_sens_ajuste", "n_spec", "n_spec_ajuste"
))
