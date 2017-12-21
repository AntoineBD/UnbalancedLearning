## Description :  différents modules qui réalisent les sous-tâches du SMOTING
## Date : 17 décembre 2018
## Author : Eric HAMMEL

## fonction qui  regroupe les différents modules qui réalisent les sous-tâches
##    du sous-échantillonage
## INPUT :
##    target : variable cible binaire à ré-échantilloner
##    data : jeu de données à ré-équilibrer
##    N : valeur en centaine pour déterminer le pourcentage de sur-échantillonage de la 
##        classe minoritaire
##    k :  nombre de plus prochaine voisin qui serviront à construire les observations
##        synthétiques
## OUTPUT :
##    jeu de donnée ou la classe minoritaire a été ré-échantillonée
## NOTE :  l'implémentation s'est inspirée de l'esprit de la fonction "SMOTE" du package DMwR,
##         cependant certaines tâche ont été adaptées aux données de grand volume
smote <- function(target, data, N = 200, k = 5)
{
  data <- integer_factors(data)
  target <- target_index(target, data)
  min_cl_ids <- minority_obs_index(target, data)
  ordered_ids <- order_index(target, data)
  new_exs <- smote_points(data = data[min_cl_ids,], N = N, k = k)
  if (target < ncol(data))
  {
    new_exs <- new_exs[,ordered_ids$cols]
    data <- data[,ordered_ids$cols]
  }
  Maj_cl <- sample((1:nrow(data))[-min_cl_ids], size = as.integer((N/100)*nrow(new_exs)),
                   replace = TRUE)
  mat <- data.frame(rbind(as.matrix(data[Maj_cl,]), as.matrix(data[min_cl_ids,]), as.matrix(new_exs)))
  return(data.frame(mat))
}

##  Implémentation de la synthétisation de des observations de la classe minoritaire
## INPUT :
##    target : variable cible binaire à ré-échantilloner
##    data : jeu de données à ré-équilibrer
##    N : valeur en centaine pour déterminer le pourcentage de sur-échantillonage de la 
##        classe minoritaire
##    k :  nombre de plus prochaine voisin qui serviront à construire les observations
##        synthétiques
## OUTPUT :
##    jeu de donnée ou la classe minoritaire a été ré-échantillonée
smote_points <- function(data, N = 200, k = 5)
{
  data.int <- integer_factors(data)
  if (N < 100)
  {
    nT <- nrow(data)
    min_cl_sample <- sample(1:nT, as.integer((N/100) * nT))
    data.int <- data.int[min_cl_sample,]
    N <- 100
  }
  N <- as.integer(N/100)
  nT <- nrow(data)
  num_attrs <- ncol(data)
  synthetic <- matrix(0,nrow = N * nT, ncol = num_attrs)
  apply(as.matrix(1:nT), MARGIN = 1, function(x){
    nn_array <- custom_knn(x, k, data)
    for (n in seq.int(N))
    {
      random_neighbor <- sample(1:k, 1)
      dif <- data[nn_array[random_neighbor],] - data[x,]
      elt <- runif(n = 1, min = 0, max = 1) * dif
      new_index <- ((x - 1) * N) + n
      synthetic[new_index,] <- as.matrix(data[x,] + elt)
    }
  })
  lapply(synthetic , function(x) factor(x, levels = 1:nlevels(x)))
  synthetic <- data.frame(synthetic)
  synthetic[,ncol(data)] <- data[1, ncol(data)]
  
  return(synthetic)
}

##  Convertir les variables qualitative multimodales non-numériques en
##  variables qualitative multimodales numériques
## INPUT :
##    data : jeu de données à ré-équilibrer
## OUTPUT :
##    colonne contenant les colonnes converties
integer_factors <- function(data)
{
  return(as.data.frame(lapply(data, function(x) if (is.numeric(x)) x else as.numeric(x))))
}

##  calcul des k plus proches voisins
## INPUT :
##    obs_i : jeu de données à ré-équilibrer
##    k : nombre de k plus proches voisins
##    data : jeu de données 
## OUTPUT :
##    les k plus proches observations de la ligne la ligne i
custom_knn <- function(obs_i, k = 5, data)
{
  dist <- pdist::pdist(data[obs_i,], data)
  return(which(dist@dist %in% head(sort(dist@dist)[2:(k + 1)], n = k)))
}

##  calcul des k plus proches voisins
## INPUT :
##    N : pourcentage de sur-échantillonage de la classe minoritaire
##    i : observation i du de la classe minoritaire qui servira à synthétiser des observations
##    nnarray : vecteur des indices des k plus proches voisins
##    data : objet contenant les observation de la classe minoritaire
##    synthetic : matrice vice qui contiendra les observation synthétisée
##    new_index : indice de la ligne suivant à remplir de la matrice synthetic
##    n.attr : nombre d'attributs/variables
##    k : nombre de voisins les plus proches
## OUTPUT :
##    les k plus proches observations de la ligne la ligne i
# populate <- function(N, i, nnarray, data, synthetic, new_index, n.attr, k)
# {
#   
#   return(list( new.ind = new_index, synthetic = synthetic))
# }

##  Uniformisation de l'appel à la variable cible en trouvant son indice
## INPUT :
##    target : nom ou indice de la variable cible
##    data : objet contenant les observation de la classe minoritaire
## OUTPUT :
##    indice de la variable cible
target_index <- function(target, data)
{
  if (is.numeric(target)) stopifnot(target <= ncol(data))
  if (is.character(target)) stopifnot(target %in% colnames(data))
  return(ifelse(is.numeric(target), target, which(colnames(data) == target)))
}

##  Détection du nom de la classe minoritaire
## INPUT :
##    target : nom ou indice de la variable cible
##    data : objet contenant les observation de la classe minoritaire
## OUTPUT :
##    indice de la variable cible
detect_minority_name <- function(target, data)
{
  if (is.numeric(target)) stopifnot(target <= ncol(data))
  if (is.character(target)) stopifnot(target %in% colnames(data))
  return(names(which.min(table(data[,target]))))
}

##  Détection du nom de la classe minoritaire
## INPUT :
##    target : nom ou indice de la variable cible
##    data : objet contenant les observation de la classe minoritaire
## OUTPUT :
##    indice de la variable cible
minority_obs_index <- function(target, data)
{
  minority_name <- detect_minority_name(target, data)
  true_false_vec <- data[,target] == minority_name
  return(which(true_false_vec == TRUE))
}

##  Met la colonne en à la fin de jeu de donnée si il ne l'est pas
## INPUT :
##    target : nom ou indice de la variable cible
##    data : objet contenant les observation de la classe minoritaire
## OUTPUT :
##    liste contenant le vecteur des indices des colonnes réarrangé (cols)
##    le jeu de données avec les colonnes réarrangées (data)
order_index <- function(target, data)
{
  if (is.numeric(target)) stopifnot(target <= ncol(data))
  ifelse(is.numeric(target), target, target <- target_index(target, data))
  if (target < ncol(data))
  {
    cols <- 1:ncol(data)
    cols[c(target, ncol(data))] <- cols[c(ncol(data), target)]
    data <- data[, cols]
    list(cols = cols, data = data)
  } else {
    return(data)
  }
}
