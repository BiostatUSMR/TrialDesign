#' Randomisation par blocs
#'
#' @description
#' Génère une liste de randomisation par blocs en attribuant à chaque sujet
#' un groupe de traitement selon un ratio donné. Les sujets sont assignés à
#' des blocs de taille définis par l'utilisateur.
#'
#' @param n nombre total de sujets
#' @param k nombre de groupes de traitement
#' @param seed graine aléatoire pour reproductibilité
#' @param block_sizes  vecteur numérique définissant la taille réélle des blocs souhaitée
#' @param nb_block vecteur numérique définissant le nombre de blocs pour chaque éléments de block_size
#' @param ratio Vecteur numérique définissant le ratio d’allocation dans chaque bloc;
#'  Si NULL, un ratio équilibré 1:1:...:1 est utilisé.
#' @param arm_label Vecteur de caractère contenant les noms des groupes de traitement;
#'  Si NULL, "Groupe1", "Groupe2"... sont utilisés.
#' @param arm_code Vecteur numérique contenant les numeros des groupes de traitement;
#'  Si NULL, 1, 2, ..., k sont utilisés.
#'
#' @returns Un dataframe contenant:l’identifiant du sujet, le numéro et le libellé du groupe de traitement assigné
#'
#' @export
#'
#' @examples rando_bloc(n = 60, k = 2, seed = 42, block_sizes = c(6), nb_block = c(10),ratio = c(2, 1),arm_label = c("Traitement", "Placebo"))


rando_bloc <- function(n,
                       k,
                       seed,
                       block_sizes,
                       nb_block,
                       ratio = NULL,
                       arm_label = NULL,
                       arm_code= NULL){

  # Valeurs par défaut
  if (is.null(ratio))     { ratio <- rep(1, k) }

  if (is.null(arm_label)) { arm_label <- paste0("Groupe", 1:k) }

  if(is.null(arm_code)){arm_code <- seq (1: k) }


  #verification des arguments de base
  if (!is.numeric(n) || n <= 0 || n != round(n)) {
    stop("'n' doit être un entier strictement positif.")}

  if (!is.numeric(k) || k < 2 || k != round(k)) {
    stop("'k' doit être un entier >= 2.")}

  if (!is.numeric(seed) || seed != round(seed)) {
    stop("'seed' doit être un entier.")}

  if (!is.numeric(block_sizes) || any(block_sizes <= 0)) {
    stop("'block_sizes' doit contenir uniquement des nombres entiers strictement positif .")}

  if (any(block_sizes %% sum(ratio) != 0)) {
    stop("Chaque taille de bloc doit être divisible par sum(ratio).")}

  if (!is.numeric(nb_block) || any(nb_block <= 0)) {
    stop("'nb_block' doit contenir uniquement des nombres entiers strictement positif.")}

  if (n != sum(block_sizes * nb_block)) {
    stop("'n' n'est pas cohérent avec block_sizes et nb_block.
        n doit être égal à sum(block_sizes * nb_block).")}

  if (length(block_sizes) != length(nb_block)) {
    stop("La longueur de 'block_sizes' doit être égale à celle de 'nb_block'.")}


  #Verification ratio si fourni
  if (!is.null(ratio)) {
    if (!is.numeric(ratio) || any(ratio <= 0)) {
      stop("'ratio' doit contenir des valeurs numériques strictement positives.")}
    if (length(ratio) != k) {
      stop("'ratio' doit être un vecteur de longueur k.")}
  }

  # Vérification arm_label si fourni
  if (!is.null(arm_label)) {
    if (length(arm_label) != k) {
      stop("La longueur de 'labe' doit être égale à k.")}
  }

  # Vérification arm_code si fourni
  if (!is.null(arm_code)) {
    if (!is.numeric(arm_code)) {
      stop("'arm_code' doit contenir des valeurs numériques.")}
    if (length(arm_code) != k) {
      stop("La longueur de 'arm_code' doit être égale à k.")}
  }


  # Fixer la graine
  set.seed(seed)

  # Génération de l'allocation
  allocation = c()

  # Liste aléatoire des m blocs avec leurs taille
  # m est e nombre total de blocs dans la liste
  m <- sum(nb_block)
  # générer une liste contenant les m tailles de blocs total
  block_list=c()
  for (i in 1:length(block_sizes)){
    x= rep(block_sizes[i], times=nb_block[i])
    block_list <- c(block_list,x)
  }
  # répartir de facon aléatoire les m blocs
  block_list <- sample(block_list)

  # Génération du contenu de chaque bloc
  for (j in 1:m){
    contenu <- rep(1:k, times = ratio/sum(ratio)*block_list[j])
    bloc <- sample(contenu)

    # Sequence finale
    allocation <- c(allocation, bloc)
  }

  # Construire le data.frame
  df <- data.frame(
    "Numéro de randomisation" = id_var <- formatC(1:n, width = nchar(n), flag = "0") ,
    "Code du traitement" = arm_code[allocation],
    "Libellé du traitement:" = arm_label[allocation],
    stringsAsFactors = FALSE
  )

  return(df)
}




