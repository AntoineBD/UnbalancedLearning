#' smoting data
#'
#' @param target nom ou indice de la variable cible
#' @param data jeu de données
#' @param N pourcentage de sur-échantillonage
#' @param k nombre de voisins les plus proches
#'
#' @return smoted data set
#'
smote <- function(target, data, N = 200, k = 5)
{
  target <- target_index(target, data)
  min_cl_ids <- minority_obs_index(target, data)
  ordered_ids <- order_index(target, data)
  new_exs <- smote_points(data = data, N = N, k = k)
  if (target < ncol(data))
  {
    new_exs <- new_exs[,ordered_ids$cols]
    data <- data[,ordered_ids$cols]
  }
  Maj_cl <- sample(1:nrow(data)[-min_cl_ids], size = as.integer(N/100)*nrow(new_exs),
                   replace = TRUE)
  return(rbind(data[Maj_cl], data[min_cl_ids], new_exs))
}

#' computing smote function
#'
#' @param data the minority class sample
#' @param N the over sample ration in hundreds
#' @param k number of nearest neighbor to compute synthetic observations
#'
#' @return smote minority class sample
#'
smote_points <- function(data, N = 200, k = 5)
{
  data.int <- integer_factors(data)
  if (N < 100)
  {
    nT <- nrow(data)
    min_cl_sample <- sample(1:nT, as.integer(N/100) * nT)
    data.int <- data.int[min_cl_sample,]
    N <- 100
  }
  N <- as.integer(N/100)
  nT <- nrow(data)
  num_attrs <- ncol(data)
  n_synt <- (nT * N) - nT
  synthetic <- matrix(nrow = n_synt ,ncol = 42)
  new_index <- 1
  
  # initializing the synthetic individuals matrix
  nn_array <- custom_knn(1, k, data)
  pop_list <- populate(N, 1, nn_array, data, synthetic, new_index, num_attrs, k)
  
  for (i in 2:nT)
  {
    nn_array <- custom_knn(i, k, data)
    pop_list <- populate(N, i, nn_array, data, pop_list$synthetic,
                         pop_list$new_index, num_attrs, k)
  }
  
  lapply(synthetic , function(x) factor(x, levels = 1:nlevels(x),
                                        labels = levels(x)))
  synthetic <- data.frame(synthetic)
  synthetic[,ncol(data)] <- factor(rep(data[1, ncol(data)], nrow(synthetic)),
                                   levels = levels(data[, ncol(data)]))
  
  return(synthetic)
}
#' Converts character factors to numeric factor
#'
#' @param data a dataset with column factor
#'
#' @return jeu de donnée où les variable multi-modales sont numériques
#'
integer_factors <- function(data)
{
  return(as.data.frame(lapply(data, function(x) if (is.numeric(x)) x else as.numeric(x))))
}

#' computing the k nearest neighbors on an observation
#'
#' @param obs_i observation i of a given dataset
#' @param k the number of k closest neighbors
#' @param data the given dataset
#'
#' @importFrom pdist pdist
#' @return jeu de donnée où les variable multi-modales sont numériques
#'
custom_knn <- function(obs_i, k = 5, data)
{
  dist <- pdist::pdist(data[obs_i,], data)
  return(which(dist@dist %in% head(sort(dist@dist)[2:(k + 1)], n = k)))
}

#' computing the k nearest neighbors on an observation
#'
#' @param N percentage of observation to synthetized
#' @param i observation of minority class sample that is computed
#' @param nnarray array of k nearest neighbors indexes
#' @param data minority class sample
#' @param synthetic empty matrix that contain the synthetized observations
#' @param new_index index of the synthetized observations
#' @param n.attr number on attributes
#' @param k number of nearest neighbor
#' @return
#'
populate <- function(N, i, nnarray, data, synthetic, new_index, n.attr, k)
{
  for (n in seq.int(N))
  {
    random_neighbor <- sample(1:k, 1)
    dif <- data[nnarray[random_neighbor,],] - data[n,]
    synthetic[new_index,] <- data[i,] + runif(n = 1, min = 0, max = 1) * dif
    new_index <- new_index + 1
  }
  return(list( new.ind = new_index, synthetic = synthetic))
}
#' Uniformisation de l'appel à la variable cible
#'
#' @param target variable cible du jeu de données
#' @param data data set that is smoted
#'
#' @return indice de la variable cible
#'
target_index <- function(target, data)
{
  if (is.numeric(target)) stopifnot(target <= ncol(data))
  if (is.character(target)) stopifnot(target %in% colnames(data))
  return(ifelse(is.numeric(target), target, which(colnames(data) == target)))
}

#' détection du nom de la classe minoritaire
#'
#' @param target indice ou nom de la colonnes contenant la variable à prédire
#' @param data jeu de donnée
#'
#' @return nom de la classe minoritaire dans u
#'
detect_minority_name <- function(target, data)
{
  if (is.numeric(target)) stopifnot(target <= ncol(data))
  if (is.character(target)) stopifnot(target %in% colnames(data))
  return(names(which.min(table(data[,target]))))
}

#' indice des individus de la classe minoritaire
#'
#' @param target indice ou nom de la colonnes contenant la variable à prédire
#' @param data jeu de donnée
#'
#' @return index des individus de la classe minoritaire
#'
minority_obs_index <- function(target, data)
{
  minority_name <- detect_minority_name(target, data)
  true_false_vec <- data[,target] == minority_name
  return(which(true_false_vec == TRUE))
}

#' Si la colonne de la variable ne se trouve pas à la fin du tableau de données
#' retourne les indices arrangés
#'
#' @param target indice ou nom de la colonnes contenant la variable à prédire
#' @param data jeu de donnée
#'
#' @return le jeu de données avec les colonnes permutées, et le vecteur contenant les indices permutés
#'
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
