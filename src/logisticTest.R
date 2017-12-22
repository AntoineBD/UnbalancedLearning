## Fichier: datasetDescription.r
## Auteur : Eric HAMMEL
## Description : Comparer les implémentations de smote
## Date : 17 décembre 2018

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


## 10-fold CV
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE)

#### UNsmoted data ####
unsmoted.model.fit <- train( connection_type ~ ., data = train.data,
                             method = "glm", trControl = fitControl, metric = "ROC")
## assessing
unsmoted.pred <- predict(unsmoted.model.fit, newdata = test.data)
confusionMatrix(data = unsmoted.pred, reference = test.data$connection_type)
## ROC curve
unsmoted.probs <- predict(unsmoted.model.fit,newdata =  test.data,
                          type = "prob")
unsmoted.roc <- roc(predictor = unsmoted.probs$bad, response = test.data$connection_type,
                    levels = rev(levels(test.data$connection_type)))
unsmoted.roc$auc # Area under the curve : 0.9293
plot(unsmoted.roc, main = "logistic regression ROC")


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

table(train.data$connection_type)





##### custom smoted data ####

custom.smote <- smote(target = "connection_type", data = train.data)
custom.smote[,"connection_type"] <- factor(custom.smote[, "connection_type"],
                                           levels = 1:nlevels(kdd.data[, "connection_type"]),
                                           labels = levels(kdd.data[, "connection_type"]))
custom.smoted.fit <- train( connection_type ~ ., data = custom.smote,
                             method = "glm", trControl = fitControl, metric = "ROC")
## assessing
custom.smoted.pred <- predict(custom.smoted.fit, newdata = test.data)
confusionMatrix(data = custom.smoted.pred, reference = test.data$connection_type)
## ROC curve
custom.smoted.probs <- predict(custom.smoted.fit,newdata =  test.data,
                          type = "prob")
custom.smoted.roc <- roc(predictor = custom.smoted.probs$bad, response = test.data$connection_type,
                         levels = rev(levels(test.data$connection_type)))
custom.smoted.roc$auc # Area under the curve : 0.9286
plot(custom.smoted.roc, main = "logistic regression ROC")

custom.smoted.pred = as.character(custom.smoted.pred)
custom.smoted.pred[custom.smoted.pred == 'good'] = 1
custom.smoted.pred[custom.smoted.pred == 'bad'] = 0

Precision_SMOTE = (sum(custom.smoted.pred == OBS))/length(custom.smoted.pred)

FN = sum((custom.smoted.pred == 0) & (OBS == 1))

VP = sum((custom.smoted.pred == 1) & (OBS == 1))

Rappel_SMOTE = VP/(FN+VP)

Gmean_SMOTE = measure_GMEAN(OBS, custom.smoted.pred, 1, 0)

FMeasure = F_Measure(Rappel_SMOTE,Precision_SMOTE,1)

table(custom.smote$connection_type)


##### Borderline-smote #####


custom.border <- BorderLineSMOTE1(train.data2, train.data2$connection_type, K=3)
# custom.border[,"connection_type"] <- factor(custom.border[, "connection_type"],
#                                            levels = 1:nlevels(as.factor(kdd.data[, "connection_type"])),
#                                            labels = levels(as.factor(kdd.data[, "connection_type"])))

custom.border$connection_type = as.character(custom.border$connection_type)
custom.border$connection_type[custom.border$connection_type == '1'] = 'good'
custom.border$connection_type[custom.border$connection_type == '0'] = 'bad'
custom.border$connection_type = as.factor(custom.border$connection_type)

custom.border.fit <- train(connection_type ~ ., data = custom.border,
                            method = "glm", trControl = fitControl, metric = "ROC")
## assessing
custom.border.pred <- predict(custom.border.fit, newdata = test.data2)
confusionMatrix(data = custom.border.pred, reference = test.data$connection_type)
## ROC curve
custom.border.probs <- predict(custom.border.fit,newdata =  test.data,
                               type = "prob")
custom.border.roc <- roc(predictor = custom.border.probs$bad, response = test.data$connection_type,
                         levels = rev(levels(test.data$connection_type)))
custom.border.roc$auc
plot(custom.border.roc, main = "logistic regression ROC")

custom.border.pred = as.character(custom.border.pred)
custom.border.pred[custom.border.pred == 'good'] = 1
custom.border.pred[custom.border.pred == 'bad'] = 0

Precision_BORDER = (sum(custom.border.pred == OBS))/length(custom.border.pred)

FN = sum((custom.border.pred == 0) & (OBS == 1))

VP = sum((custom.border.pred == 1) & (OBS == 1))

Rappel_BORDER = VP/(FN+VP)

Gmean_BORDER = measure_GMEAN(OBS, custom.border.pred, 1, 0)

FMeasure = F_Measure(Rappel_BORDER,Precision_BORDER,1)

table(custom.border$connection_type)


##### ADASYN data ####

custom.adasyn <- ADASYN(train.data[,-ncol(train.data)],train.data[,ncol(train.data)],0.75,1,10)

custom.adasyn.fit <- train(connection_type ~ ., data = data.frame(custom.adasyn$data,connection_type = custom.adasyn$y),
                            method = "glm", trControl = fitControl, metric = "ROC")
## assessing
custom.adasyn.pred <- predict(custom.adasyn.fit, newdata = test.data)
confusionMatrix(data = custom.adasyn.pred, reference = test.data$connection_type)
## ROC curve
custom.adasyn.probs <- predict(custom.smoted.fit,newdata =  test.data,
                               type = "prob")
custom.adasyn.roc <- roc(predictor = custom.adasyn.probs$bad, response = test.data$connection_type,
                         levels = rev(levels(test.data$connection_type)))
custom.adasyn.roc$auc
plot(custom.adasyn.roc, main = "logistic regression ROC")


custom.adasyn.pred = as.character(custom.adasyn.pred)
custom.adasyn.pred[custom.adasyn.pred == 'good'] = 1
custom.adasyn.pred[custom.adasyn.pred == 'bad'] = 0

Precision_ADASYN = (sum(custom.adasyn.pred == OBS))/length(custom.adasyn.pred)

FN = sum((custom.adasyn.pred == 0) & (OBS == 1))

VP = sum((custom.adasyn.pred == 1) & (OBS == 1))

Rappel_ADASYN = VP/(FN+VP)

Gmean_ADASYN = measure_GMEAN(OBS, custom.adasyn.pred, 1, 0)

FMeasure = F_Measure(Rappel_ADASYN,Precision_ADASYN,1)

table(custom.adasyn$y)


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
