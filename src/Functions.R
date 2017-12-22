#########################################################################################
#########################################################################################
####################### Fichier des Fonctions et des librairies ##########################
#########################################################################################
#########################################################################################

### Quelques library

library(doParallel)
library(vcd)
library("e1071")
library(SDMTools)

library(smotefamily)
library(KernelKnn)

library("caret")
library("pROC")


#### Fonction d'importation des données ####



## Importation du jeu de données kddcup en utilisant la librairie data.table qui 
## est adpatée à un fichier de cette taille
## INPUT : pas d'INPUT
## OUTPUT :
##  le jeu de données de type data.table
createDataset <- function()
{
  library("data.table") # library adapting dataframes for large datasets
  # setting up header
  dtHeader <- read.table("data/kddcup.header.txt", sep = ",", header = TRUE)
  
  # loading dataset without the header
  kddcup.dataset <- fread(input = "data/kddcup.data.corrected", sep = ",",
                          header = FALSE, col.names = names(dtHeader),
                          stringsAsFactors = TRUE)
  # removing the point from attack_type factor
  kddcup.dataset$attack_type <- as.factor(gsub(".$","",kddcup.dataset$attack_type))
  return(kddcup.dataset)
}



##### Fonctions pour le choix des variables qualitatives #####



## Sequentielle

# CRAMER.value = function(data, vec){
#   
#   CRAMER.res = rep(0,length(vec))
#   k = 0
#   
#   for(i in vec){
#     k = k + 1
#     
#     CRAMER.res[k] =
#       assocstats(table(as.numeric(data[,i]),as.numeric(data$connection_type)))$cramer
#   }
#   return(cbind(vec,CRAMER.res))
# }

## Vectorielle

CRAMER.value.vec = function(data, vec){
  
  Into.sapply = function(x){
    
    return(assocstats(table(as.numeric(data[,x]),as.numeric(data$connection_type)))$cramer)
  }
  
  CRAMER.res = sapply(vec, FUN = Into.sapply)
  
  return(cbind(vec,CRAMER.res))
}

# ## Vectorielle para
# 
# CRAMER.value.vec.para = function(cl, data, vec){
#   
#   clusterExport(cl, c("assocstats","Test_3"))
#   
#   Into.sapply = function(x){
#     
#     return(assocstats(table(as.numeric(data[,x]),as.numeric(data$connection_type)))$cramer)
#   }
#   
#   CRAMER.res = parSapply(cl, vec, FUN = Into.sapply)
#   
#   return(cbind(vec,CRAMER.res))
# }



##### Fonction pour le choix des variables quantitatives #####



Indep.quanti.quali = function(data,vec2){
  t = sapply(vec2, function(x) (kruskal.test(data[,x] ~ data$connection_type)$p.value < 0.05))
  return(data.frame(Indice = vec2, Dependance = t))
}



##### Mesure Gmean #####



measure_GMEAN = function(obs, pred, negative, positive) {
  sqrt(measure_TPR(obs, pred, positive) * measure_TNR(obs, pred, negative))
}

measure_TPR = function(obs, pred, positive) {
  measure_TP(obs, pred, positive) / sum(obs == positive)
}

measure_TP = function(obs, pred, positive) {
  sum(obs == pred & pred == positive)
}

measure_TNR = function(obs, pred, negative) {
  measure_TN(obs, pred, negative) / sum(obs == negative)
}

measure_TN = function(obs, pred, negative) {
  sum(obs == pred & pred == negative)
}



#### Fonction adasyn #####



ADASYN = function(data, var_pred, d_th, beta, K){
  
  # m = nrow(data) # nb d obs
  
  classes_pred = table(var_pred) # nb d obs par classe
  classe_min = names(which.min(classes_pred)) # nb d obs classe min
  classe_maj = names(which.max(classes_pred)) # nb d obs classe maj
  
  Obs_classe_min = data[var_pred == classe_min,]
  Obs_classe_maj = data[var_pred == classe_maj,]
  
  ind_classe_min = which(var_pred == classe_min)
  ind_classe_maj = which(var_pred == classe_maj)
  
  ms = nrow(Obs_classe_min)
  ml = nrow(Obs_classe_maj)
  
  d = ms/ml # ratio du desequilibre
  
  while( d < d_th){
    
    G = (ml - ms)*beta
    
    Delta = rep(0,ms)
    
    ind_knn = matrix(0,ms,K)
    
    for( i in 1:ms){
      
      ind_knn[i,] = knn.index.dist(data, Obs_classe_min[i,],  k = K,
                                   transf_categ_cols = FALSE)$test_knn_idx
      
      Delta[i] = sum(ind_knn[i,] %in% ind_classe_maj)
    }
    
    R = Delta / K
    
    R_norm = R/sum(R)
    
    g = round(R_norm * G)
    
    S = Obs_classe_min[0,]
    
    fact = sapply(1:ncol(data), FUN = function(x) is.factor(data[1,x]))
    
    for( i in 1:ms){
      
      g_i = g[i]
      
      if( g[i] != 0){
        
        ind_knn_min = which(ind_knn[i,] %in% ind_classe_min)
        
        s = Obs_classe_min[i,]
        
        for( j in 1:g_i){
          
          x_zi = data[sample(ind_knn[i,ind_knn_min])[1],]
          
          s[!fact] = Obs_classe_min[i,!fact] +
            (x_zi[!fact] - Obs_classe_min[i,!fact])*runif(1,0,1)
          
          S = rbind(S,s)
          
        }
      }
    }
    
    data = rbind(data,S)
    var_pred = as.character(var_pred)
    var_pred = c(var_pred,rep(classe_min,nrow(S)))
    var_pred = as.factor(var_pred)
    
    # m = nrow(data) # nb d obs du nouveau jeu
    
    Obs_classe_min = data[var_pred == classe_min,]
    Obs_classe_maj = data[var_pred == classe_maj,]
    
    ind_classe_min = which(var_pred == classe_min)
    ind_classe_maj = which(var_pred == classe_maj)
    
    ms = nrow(Obs_classe_min)
    ml = nrow(Obs_classe_maj)
    
    d = ms/ml # ratio du desequilibre
    
  }
  
  return(list(data = data, y = var_pred))
  
}

##### Fonction Smote #####


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




##### Fonction borderline-smote #####


BorderLineSMOTE1 <-function (data, target, K = 5, C = 5, dupSize = 0)
{
  obeservationsNumber = ncol(data) # le nombre d'observations
  targetTable = table(target) # la variable à prédire
  
  minorityName = names(which.min(targetTable))
  minorityData = subset(data, target == minorityName)[sample(min(targetTable)), ] # classe minoritaire
  minorityRowsNumber = nrow(minorityData)
  
  majorityData = subset(data, target != minorityName) #classe majoritaire
  majorityRowsNumber = nrow(majorityData)
  
  allData = rbind(minorityData, majorityData)
  
  knear_D = knearest(allData, minorityData, C)
  knct = kncount(knear_D, minorityRowsNumber)
  
  # dinstinger la zone de danger et la zone safe
  zonesData = knct[, 1] / C
  dangerIndexes = which(zonesData <= 0.5 & zonesData > 0)
  safeIndexes = which(zonesData > 0.5)
  
  dangerMinorityData = minorityData[dangerIndexes,]
  dangerMinorityDataRowsNumber = nrow(dangerMinorityData)
  safeMinorityData = minorityData[safeIndexes,]
  # on prend pas les données dans la zone = 0
  
  if (K >= dangerMinorityDataRowsNumber) {
    stop("Prenez une valeur iférieure pour la paramètre K")
  }
  
  dupSum = n_dup_max(
    minorityRowsNumber + majorityRowsNumber,
    dangerMinorityDataRowsNumber,
    majorityRowsNumber,
    dupSize
  )
  syntheticData = NULL
  
  knear_P = knearest(dangerMinorityData, dangerMinorityData, K)
  for (i in 1:dangerMinorityDataRowsNumber) {
    pair_idx = knear_P[i, ceiling(runif(dupSum) * K)]
    g = mapply(gap,
               sl_p = rep(1, dupSum),
               sl_n = rep(1, dupSum))
    P_i = matrix(unlist(dangerMinorityData[i,]),
                 dupSum,
                 obeservationsNumber,
                 byrow = TRUE)
    Q_i = as.matrix(dangerMinorityData[pair_idx,])
    syn_i = P_i + g * (Q_i - P_i)
    syntheticData = rbind(syntheticData, syn_i)
  }
  
  minorityData[, obeservationsNumber + 1] = rep(minorityName, minorityRowsNumber)
  colnames(minorityData) = c(colnames(data), "class")
  
  majorityData[, obeservationsNumber + 1] = target[target != minorityName]
  colnames(majorityData) = c(colnames(data), "class")
  
  rownames(syntheticData) = NULL
  syntheticData = data.frame(syntheticData)
  syntheticData[, obeservationsNumber + 1] = rep(minorityName, nrow(syntheticData))
  colnames(syntheticData) = c(colnames(data), "class")
  
  # Les données finaux (minority + synthetic + majority)
  newData = rbind(minorityData, syntheticData, majorityData)
  rownames(newData) = NULL
  
  return(newData)
}
