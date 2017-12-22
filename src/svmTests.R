
############################################################################
############################################################################
#########################          SVM            ##########################
############################################################################
############################################################################


rm(list = ls())

source("src/Functions.R", local = TRUE)


kdd.data = read.table(file = "data/mydata.txt", sep=",")

test.data = kdd.data[3800:5000,]

train.data = kdd.data[1:3799,]


kdd.data2 = kdd.data

kdd.data2$connection_type = as.character(kdd.data2$connection_type)
kdd.data2$connection_type[kdd.data2$connection_type == 'good'] = 1
kdd.data2$connection_type[kdd.data2$connection_type == 'bad'] = 0
kdd.data2$connection_type = as.numeric(kdd.data2$connection_type)

test.data2 = kdd.data2[3800:5000,]

train.data2 = kdd.data2[1:3799,]

OBS = test.data2$connection_type


#### UNsmoted data ####
unsmoted.model.fit <- svm( connection_type ~ ., data = train.data, kernel ='linear',probability=TRUE)
## assessing
unsmoted.pred <- predict(unsmoted.model.fit, newdata = test.data)
confusionMatrix(data = unsmoted.pred, reference = test.data$connection_type)
## ROC curve
unsmoted.probs <- attr(predict(unsmoted.model.fit,newdata =  test.data,probability=TRUE), "probabilities")
unsmoted.roc <- roc(predictor = unsmoted.probs[,1], response = test.data$connection_type,
                    levels = rev(levels(test.data$connection_type)))

AUC = as.numeric(unsmoted.roc$auc)
plot(unsmoted.roc, main = "svm ROC", xlim = c(1,0), ylim = c(0,1), asp = NA)


unsmoted.pred = as.character(unsmoted.pred)
unsmoted.pred[unsmoted.pred == 'good'] = 1
unsmoted.pred[unsmoted.pred == 'bad'] = 0

# confusion.matrix(OBS, unsmoted.pred ,threshold = 0.5)

Precision = (sum(unsmoted.pred == OBS))/length(unsmoted.pred)

FN = sum((unsmoted.pred == 0) & (OBS == 1))

VP = sum((unsmoted.pred == 1) & (OBS == 1))

Rappel = VP/(FN+VP)

Gmean = measure_GMEAN(OBS, unsmoted.pred, 1, 0)

FMeasure = F_Measure(Rappel,Precision,1)

prop.table(table(train.data$connection_type))





##### custom smoted data ####

custom.smote <- smote(target = "connection_type", data = train.data)
custom.smote[,"connection_type"] <- factor(custom.smote[, "connection_type"],
                                           levels = 1:nlevels(kdd.data[, "connection_type"]),
                                           labels = levels(kdd.data[, "connection_type"]))
custom.smoted.fit <- svm( connection_type ~ ., data = custom.smote, kernel ='linear',decision.values = TRUE, probability = TRUE)
## assessing
custom.smoted.pred <- predict(custom.smoted.fit, newdata = test.data)
confusionMatrix(data = custom.smoted.pred, reference = test.data$connection_type)
## ROC curve
custom.smoted.probs <- attr(predict(custom.smoted.fit,newdata =  test.data,probability=TRUE), "probabilities")
custom.smoted.roc <- roc(predictor = custom.smoted.probs[,1], response = test.data$connection_type,
                        levels = rev(levels(test.data$connection_type)))


AUC_SMOTE = as.numeric(custom.smoted.roc$auc)
plot(custom.smoted.roc, main = "svm ROC", xlim = c(1,0), ylim = c(0,1), asp = NA, col ='blue')

custom.smoted.pred = as.character(custom.smoted.pred)
custom.smoted.pred[custom.smoted.pred == 'good'] = 1
custom.smoted.pred[custom.smoted.pred == 'bad'] = 0

Precision_SMOTE = (sum(custom.smoted.pred == OBS))/length(custom.smoted.pred)

FN_S = sum((custom.smoted.pred == 0) & (OBS == 1))

VP_S = sum((custom.smoted.pred == 1) & (OBS == 1))

Rappel_SMOTE = VP_S/(FN_S+VP_S)

Gmean_SMOTE = measure_GMEAN(OBS, custom.smoted.pred, 1, 0)

FMeasure_SMOTE = F_Measure(Rappel_SMOTE,Precision_SMOTE,1)

prop.table(table(custom.smote$connection_type))

barplot(sort(table(custom.smote$connection_type), decreasing = TRUE))





##### Borderline-smote #####


custom.border <- BorderLineSMOTE1(train.data2, train.data2$connection_type, K=3)
# custom.border[,"connection_type"] <- factor(custom.border[, "connection_type"],
#                                            levels = 1:nlevels(as.factor(kdd.data[, "connection_type"])),
#                                            labels = levels(as.factor(kdd.data[, "connection_type"])))

custom.border$connection_type = as.character(custom.border$connection_type)
custom.border$connection_type[custom.border$connection_type == '1'] = 'good'
custom.border$connection_type[custom.border$connection_type == '0'] = 'bad'
custom.border$connection_type = as.factor(custom.border$connection_type)

custom.border.fit <- svm(connection_type ~ ., data = custom.border, kernel = 'linear',probability=TRUE)
## assessing
custom.border.pred <- predict(custom.border.fit, newdata = test.data2)
confusionMatrix(data = custom.border.pred, reference = test.data$connection_type)
## ROC curve
custom.border.probs <- attr(predict(custom.border.fit,newdata =  test.data,probability=TRUE), "probabilities")
custom.border.roc <- roc(predictor = custom.border.probs[,1], response = test.data$connection_type,
                         levels = rev(levels(test.data$connection_type)))


AUC_BORDER = as.numeric(custom.border.roc$auc)
plot(custom.border.roc, main = "svm ROC", xlim = c(1,0), ylim = c(0,1), asp = NA, col='red')

custom.border.pred = as.character(custom.border.pred)
custom.border.pred[custom.border.pred == 'good'] = 1
custom.border.pred[custom.border.pred == 'bad'] = 0

Precision_BORDER = (sum(custom.border.pred == OBS))/length(custom.border.pred)

FN_B = sum((custom.border.pred == 0) & (OBS == 1))

VP_B = sum((custom.border.pred == 1) & (OBS == 1))

Rappel_BORDER = VP_B/(FN_B+VP_B)

Gmean_BORDER = measure_GMEAN(OBS, custom.border.pred, 1, 0)

FMeasure_BORDER = F_Measure(Rappel_BORDER,Precision_BORDER,1)

prop.table(table(custom.border$connection_type))

barplot(sort(table(custom.border$connection_type), decreasing = TRUE))





##### ADASYN data ####

custom.adasyn <- ADASYN(train.data[,-ncol(train.data)],train.data[,ncol(train.data)],0.75,1,10)

custom.adasyn.fit <- svm(connection_type ~ ., data = data.frame(custom.adasyn$data,connection_type = custom.adasyn$y),
                           kernel = 'linear',probability=TRUE)
## assessing
custom.adasyn.pred <- predict(custom.adasyn.fit, newdata = test.data)
confusionMatrix(data = custom.adasyn.pred, reference = test.data$connection_type)
## ROC curve
custom.adasyn.probs <- attr(predict(custom.adasyn.fit,newdata =  test.data,probability=TRUE), "probabilities")
custom.adasyn.roc <- roc(predictor = custom.adasyn.probs[,1], response = test.data$connection_type,
                         levels = rev(levels(test.data$connection_type)))


AUC_ADASYN = as.numeric(custom.adasyn.roc$auc)
plot(custom.adasyn.roc, main = "svm ROC", xlim = c(1,0), ylim = c(0,1), asp = NA, col = 'green')


custom.adasyn.pred = as.character(custom.adasyn.pred)
custom.adasyn.pred[custom.adasyn.pred == 'good'] = 1
custom.adasyn.pred[custom.adasyn.pred == 'bad'] = 0

Precision_ADASYN = (sum(custom.adasyn.pred == OBS))/length(custom.adasyn.pred)

FN_AD = sum((custom.adasyn.pred == 0) & (OBS == 1))

VP_AD = sum((custom.adasyn.pred == 1) & (OBS == 1))

Rappel_ADASYN = VP_AD/(FN_AD+VP_AD)

Gmean_ADASYN = measure_GMEAN(OBS, custom.adasyn.pred, 1, 0)

FMeasure_ADASYN = F_Measure(Rappel_ADASYN,Precision_ADASYN,1)

prop.table(table(custom.adasyn$y))

barplot(sort(table(custom.adasyn$y), decreasing = TRUE))


# #### DMwR smote ####
# library("DMwR")
# dmwr.smote <- SMOTE(connection_type~., data = train.data)
# dmwr.smoted.fit <- train( connection_type ~ ., data = dmwr.smote,
#                             method = "glm", trControl = fitControl, metric = "ROC")
# ## assessing
# dmwr.smoted.pred <- predict(dmwr.smoted.fit, newdata = test.data)
# confusionMatrix(data = dmwr.smoted.fit, reference = test.data$connection_type)
# ## Roc curves
# dmwr.smoted.probs <- predict(dmwr.smoted.fit,newdata =  test.data, type = "prob")
# dmwr.smoted.roc <- roc(predictor = dmwr.smoted.probs$bad, response = test.data$connection_type,
#                          levels = rev(levels(test.data$connection_type)))
# dmwr.smoted.roc$auc # Area under the curve : 0.9359
# plot(dmwr.smoted.roc, main = "logistic regression ROC")
# 
# 
# #### Comparaison de la courbe ROC avec les différents jeux d'apprentissage ####
# plot(custom.smoted.roc, main = "logistic regression ROC", col = "blue")
# lines(custom.smoted.roc, col = "red")
# lines(dmwr.smoted.roc, col = "green")
# 
# #### Comparaison des courbes de gain ####
# lift.results <- data.frame(Class = test.data$connection_type)
# lift.results$unsmotted <- unsmoted.probs[,"bad"]
# lift.results$custom <- custom.smoted.probs[,"bad"]
# lift.results$dmwr <- dmwr.smoted.probs[,"bad"]
# 
# trellis.par.set(caretTheme())
# lift_obj <- lift(Class ~ unsmotted + custom + dmwr, data = lift.results)
# # plot(lift_obj, values = 60, auto.key = list(columns = 3,
# #                                             lines = TRUE,
# #                                             points = FALSE))
# ggplot(lift_obj, values = 75)









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
