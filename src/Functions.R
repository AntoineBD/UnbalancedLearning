#########################################################################################
#########################################################################################
################################# Fichier des Fonctions #################################
#########################################################################################
#########################################################################################



#### Fonction d'importation ####



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


