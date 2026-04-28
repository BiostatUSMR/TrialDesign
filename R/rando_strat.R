#' Randomisation Stratifiée par Blocs
#'
#'@description
#' Génère une liste de randomisation stratifiée en appliquant une
#' randomisation par blocs dans chaque strate. Les strates sont définies
#' par l'utilisateur via leur nombre et leurs labels.
#'
#' @param n nombre total de sujets
#' @param k nombre de groupes de traitement
#' @param strat nombre de strates
#' @param seed graine aléatoire pour reproductibilité
#' @param block_sizes  vecteur numérique définissant la taille réélle des blocs souhaitée
#' @param nb_block vecteur numérique définissant le nombre de blocs pour chaque éléments de block_size
#' @param ratio Vecteur numérique définissant le ratio d’allocation dans chaque bloc;
#'  Si NULL, un ratio équilibré 1:1:...:1 est utilisé.
#' @param arm_label Vecteur de caractère contenant les noms des groupes de traitement;
#'  Si NULL, "Groupe1", "Groupe2"... sont utilisés.
#' @param arm_code Vecteur numérique contenant les numeros des groupes de traitement;
#'  Si NULL, 1, 2, ..., k sont utilisés.
#' @param strat_label Vecteur de caractère contenant les noms des strates;
#'  Si NULL, "Strate1", "Strate2"... sont utilisés.
#' @param strat_code Vecteur numérique contenant les numeros des strates;
#'  Si NULL, 1, 2, ..., strat sont utilisés.
#'
#' @returns n dataframe contenant:l’identifiant du sujet, le numéro et le libellé du groupe de traitement assigné,
#' ainsi que le code et le libellé de la strate d'appartenance ,
#'
#' @export
#'
#' @examples rando_strat(n = 40, k = 2, strat = 3, seed = 42,block_sizes = c(4), nb_block = c(10),
#' strat_label = c("CentreA", "CentreB", "CentreC"))
#'


rando<- function(k,
                  seed,
                  block_sizes,
                  nb_block,
                  strat= NULL,
                  ratio = NULL,
                  arm_label = NULL,
                  arm_code= NULL,
                  strat_label= NULL,
                  strat_code=NULL){


  if (!is.null(strat)){
    #Valeurs par défaut
    if (is.null(strat_label)) { strat_label <- paste0("Strate", 1:strat) }

    if(is.null(strat_code)){strat_code <- seq (1: strat) }

    #Vérification des arguments

    if (!is.numeric(strat) || strat <= 0 || strat != round(strat)) {
      stop("'n' doit être un entier strictement positif.")}

    # Vérification strat_label si fourni
    if (!is.null(strat_label)) {
      if (length(strat_label) != strat) {
        stop("La longueur de 'strat_label' doit être égale à strat.")}
    }

    # Vérification arm_code si fourni
    if (!is.null(strat_code)) {
      if (!is.numeric(strat_code)) {
        stop("'strat_code' doit contenir des valeurs numériques.")}
      if (length(strat_code) != strat) {
        stop("La longueur de 'strat_code' doit être égale à strat.")}
    }

  }


  #Génération des listes par strates

  n <- sum(block_sizes * nb_block)
  df <- data.frame()

  if (is.null(strat)){

    df <- rando_bloc(n, k, seed, block_sizes, nb_block, ratio, arm_label, arm_code)
  }

  if (!is.null(strat)){
    for(i in 1: strat){
      #Géneration de la liste contenant que les blocs
      dfi <- rando_bloc(n, k, seed, block_sizes, nb_block, ratio, arm_label, arm_code)
      #Ajout des colonnes de strates
      dfi$RDSTR  <- strat_code[i]
      dfi$RDSTR_LIB <- strat_label[i]
      #Fusion des listes
      df <- rbind(df, dfi)
    }
  }

  return(df)


}
