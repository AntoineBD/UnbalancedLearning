
############################################################################
############################################################################
#########################          SVM            ##########################
############################################################################
############################################################################

### Quelques library

library(doParallel)
library(vcd)
library("e1071")
library(SDMTools)

library(smotefamily)
library(KernelKnn)


#### Executer dataframeSetting.




### Importation de nos donnees.

source("src/dataframeSetting.R", local = TRUE)

Test = createDataset()

Test_2 = Test

# str(Test_2)





### Modification de nos donnees, variable a prédire en 'bad'/'good'.

Test_2 = data.frame(Test_2, connection_type = 'bad')

Test_2$connection_type = as.character(Test_2$connection_type)

# Test_2$attack_type[Test_2$attack_type != 'normal'] = 'bad'

Test_2$connection_type[Test_2$attack_type == 'normal'] = 'good'

Test_2$connection_type = as.factor(Test_2$connection_type)





### Visualisation du desequilibre des classes.

barplot(sort(table(Test_2$connection_type), decreasing = TRUE))

prop.table(table(Test_2$connection_type))





### Preparation de nos donnees.

Test_3 = Test_2[sample(nrow(Test_2)),]

Test_3 = Test_3[, -c(7,8,9,11,14,15,18,20,21,(ncol(Test_3)-1))] # Sup vars inutiles


## Selection d un echantillon

n = 30001

Base_test = Test_3[(n+1):floor(n*0.3),]

Test_3 = Test_3[1:n,]


## Selection des vars/tests de dependance.

# ks.test(as.numeric(Test_3$logged_in),as.numeric(Test_3$connection_type))
# chisq.test(as.numeric(Test_3$logged_in),as.numeric(Test_3$connection_type))
# 
# TB_CT = table(as.numeric(Test_3$logged_in),as.numeric(Test_3$connection_type))
# 
# assocstats(TB_CT)  ## Cramer V = 0.819, grosse association.

#### Test pour quali/quali -> 2,3,4,8,13

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

# cl <- makeCluster(2)
# registerDoParallel(cl)
# system.time(CRAMER.value(Test_3, c(2,3,4,8,13)))
# system.time(CRAMER.value.vec(Test_3, c(2,3,4,8,13)))
# system.time(CRAMER.value.vec.para(cl, Test_3, c(2,3,4,8,13)))
# stopCluster(cl)

CRAMER.value.vec(Test_3, c(2,3,4,8,13))

## Au vu des resultats, 3 et 8 sont tres dep avec la var de sortie.
## 2 un peu


#### Test pour quanti/quali, test difference deux echs, sans hyp sur la distribution.

MEAN.classe = aggregate(x = Test_3[,-c(2,3,4,8,13, ncol(Test_3))],
                        by = list(Test_3$connection_type), FUN = mean)

MEDIAN.classe = aggregate(x = Test_3[,-c(2,3,4,8,13, ncol(Test_3))],
                          by = list(Test_3$connection_type), FUN = median)

## Au vu des resultats, dst_bytes, count, svr_count et dst_host_same_src_port_rate
## sont tres dep avec la var de sortie.

# dst_host_count, dst_host_svr_diff_host_rate, dst_host_diff_svr_rate, dst_host_srv_serror_rate.


### Jeu de donnees pour modele svm

DATA.svm = Test_3[, -c(3,8, 2, 6,14,15,27, 23,26,28,30, 19:22)]

Base_test.svm = Base_test[, -c(3,8, 2, 6,14,15,27, 23,26,28,30, 19:22)]


## Peut-être aussi 24, 25


########################### MODELE SVM/PREDICTIONS ###########################


Mod_SVM = svm(connection_type~., data = DATA.svm, kernel="linear")

Pred = predict(Mod_SVM, Base_test.svm[,-ncol(Base_test.svm)])


### Matrice de confusion et pourcentage de bonnes predictions.

Pred = as.character(Pred)
Pred[Pred == 'good'] = 1
Pred[Pred == 'bad'] = 0

OBS = as.character(Base_test.svm[,ncol(Base_test.svm)])
OBS[OBS == 'good'] = 1
OBS[OBS == 'bad'] = 0

conf_matrix = confusion.matrix(OBS, Pred, threshold = 0.5)

(sum(Pred == OBS))/length(Pred)





########## Utilisation de la Mesure Gmean

# library(mlr)

measureGMEAN = function(truth, response, negative, positive) {
  sqrt(measureTPR(truth, response, positive) * measureTNR(truth, response, negative))
}

measureTPR = function(truth, response, positive) {
  measureTP(truth, response, positive) / sum(truth == positive)
}

measureTP = function(truth, response, positive) {
  sum(truth == response & response == positive)
}

measureTNR = function(truth, response, negative) {
  measureTN(truth, response, negative) / sum(truth == negative)
}

measureTN = function(truth, response, negative) {
  sum(truth == response & response == negative)
}

Gmean = measureGMEAN(OBS, Pred, 1, 0)

############################################################################
############################################################################
#########################          ADASYN         ##########################
############################################################################
############################################################################


## sup var quali.

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
      
      Delta[i] = sum(ind_knn[i,] %in% ind_classe_maj) # Classe maj ou min ?
    }
    
    R = Delta / K
    
    R_norm = R/sum(R)
    
    g = round(R_norm * G)
    
    S = Obs_classe_min[0,] ### revoir la taille de s
    
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


## ######################### Test fct ############################ ##

# u = DATA.svm[sample(1:nrow(DATA.svm)),-c(2,5,8)]
# 
# u = u[1:5000,]
# 
# u_val = u[3800:5000,]
# 
# u = u[1:3799,]

data = rbind(u,u_val)

write.table(data, "mydata.txt", sep=",")

v = read.table(file = "mydata.txt", sep=",")

v_val = v[3800:5000,]

v = v[1:3799,]

Mod_SVM = svm(connection_type~., data = u, kernel="linear")

Pred = predict(Mod_SVM, u_val[,-ncol(u_val)])


### Matrice de confusion et pourcentage de bonnes predictions.

Pred = as.character(Pred)
Pred[Pred == 'good'] = 1
Pred[Pred == 'bad'] = 0

OBS = as.character(u_val[,ncol(u_val)])
OBS[OBS == 'good'] = 1
OBS[OBS == 'bad'] = 0

(sum(Pred == OBS))/length(Pred)

Gmean = measureGMEAN(OBS, Pred, 1, 0)

##################################################################

P = ADASYN(u[,-ncol(u)],u[,ncol(u)],0.75,1,10)

Mod_SVM = svm(connection_type~., data = data.frame(P$data,connection_type = P$y), kernel="linear")

Pred = predict(Mod_SVM, u_val[,-ncol(u_val)])


### Matrice de confusion et pourcentage de bonnes predictions.

Pred = as.character(Pred)
Pred[Pred == 'good'] = 1
Pred[Pred == 'bad'] = 0

OBS = as.character(u_val[,ncol(u_val)])
OBS[OBS == 'good'] = 1
OBS[OBS == 'bad'] = 0

(sum(Pred == OBS))/length(Pred)

Gmean2 = measureGMEAN(OBS, Pred, 1, 0)


##################################################################

P_2 = ADAS(u[,-ncol(u)],u[,ncol(u)],K = 10)

Mod_SVM = svm(class~., data = P_2$data, kernel="linear", type='C')

Pred = predict(Mod_SVM, u_val[,-ncol(u_val)])


### Matrice de confusion et pourcentage de bonnes predictions.

Pred = as.character(Pred)
Pred[Pred == 'good'] = 1
Pred[Pred == 'bad'] = 0

OBS = as.character(u_val[,ncol(u_val)])
OBS[OBS == 'good'] = 1
OBS[OBS == 'bad'] = 0

(sum(Pred == OBS))/length(Pred)

Gmean3 = measureGMEAN(OBS, Pred, 1, 0)
