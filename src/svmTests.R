
############################################################################
############################################################################
#########################          SVM            ##########################
############################################################################
############################################################################

source("src/Functions.R", local = TRUE)

#### Importation de nos donnees. ####
 

data = read.table(file = "data/mydata.txt", sep=",")

data_val = data[3800:5000,]

data = data[1:3799,]


#### Visualisation du desequilibre des classes. ####

# barplot(sort(table(data$connection_type), decreasing = TRUE))
# 
# prop.table(table(data$connection_type))



########################### MODELE SVM/PREDICTIONS ###########################



Mod_SVM = svm(connection_type~., data = data, kernel="linear")

Pred = predict(Mod_SVM, data_val[,-ncol(data_val)])


### Matrice de confusion et pourcentage de bonnes predictions.

Pred = as.character(Pred)
Pred[Pred == 'good'] = 1
Pred[Pred == 'bad'] = 0

OBS = as.character(data_val[,ncol(data_val)])
OBS[OBS == 'good'] = 1
OBS[OBS == 'bad'] = 0

P_pred = (sum(Pred == OBS))/length(Pred)

conf_matrix = confusion.matrix(OBS, Pred, threshold = 0.5)

Gmean = measure_GMEAN(OBS, Pred, 1, 0)


############################# Avec ADASYN #############################


P = ADASYN(data[,-ncol(data)],data[,ncol(data)],0.75,1,10)

Mod_SVM = svm(connection_type~., data = data.frame(P$data,connection_type = P$y), kernel="linear")

Pred = predict(Mod_SVM, data_val[,-ncol(data_val)])


### Matrice de confusion et pourcentage de bonnes predictions.

Pred = as.character(Pred)
Pred[Pred == 'good'] = 1
Pred[Pred == 'bad'] = 0

OBS = as.character(data_val[,ncol(data_val)])
OBS[OBS == 'good'] = 1
OBS[OBS == 'bad'] = 0

P_pred_ADASYN = (sum(Pred == OBS))/length(Pred)

conf_matrix = confusion.matrix(OBS, Pred, threshold = 0.5)

Gmean_ADASYN = measure_GMEAN(OBS, Pred, 1, 0)


###################### Avec ADASYN de smotefamily #####################


P_2 = ADAS(data[,-ncol(data)],data[,ncol(data)],K = 10)

Mod_SVM = svm(class~., data = P_2$data, kernel="linear", type='C')

Pred = predict(Mod_SVM, data_val[,-ncol(data_val)])


### Matrice de confusion et pourcentage de bonnes predictions.

Pred = as.character(Pred)
Pred[Pred == 'good'] = 1
Pred[Pred == 'bad'] = 0

OBS = as.character(data_val[,ncol(data_val)])
OBS[OBS == 'good'] = 1
OBS[OBS == 'bad'] = 0

P_pred_ADASYN_R = (sum(Pred == OBS))/length(Pred)

Gmean_ADASYN_R = measure_GMEAN(OBS, Pred, 1, 0)















ADASYN_vec = function(data, var_pred, d_th, beta, K){
  
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
    
    # Delta.ind_knn = function(x){
    #     u = knn.index.dist(data, Obs_classe_min[x,],  k = K,
    #                                   transf_categ_cols = FALSE)$test_knn_idx
    #     ind_knn = u
    #     
    #     Delta = apply(ind_knn, 1, function(y) sum(y %in% ind_classe_maj))
    # 
    #     return(c(ind_knn,Delta))
    # }
    # 
    # Res = sapply(1:ms, Delta.ind_knn)
    # 
    # ind_knn = t(Res[-nrow(Res),])
    # 
    # Delta = Res[nrow(Res),]
    
    ind_knn_fct =  function(x){
      ind_knn = knn.index.dist(data, Obs_classe_min[x,],  k = K,
                                   transf_categ_cols = FALSE)$test_knn_idx

      return(ind_knn)
    }

    Delta_fct =  function(x){

      Delta = sum(ind_knn[x,] %in% ind_classe_maj)

      return(Delta)
    }

    ind_knn = t(sapply(1:ms, ind_knn_fct))

    Delta = sapply(1:ms, Delta_fct)
    
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

system.time(ADASYN(data[,-ncol(data)],data[,ncol(data)],0.75,1,10))
system.time(ADASYN_vec(data[,-ncol(data)],data[,ncol(data)],0.75,1,10))
