
############################################################################
############################################################################
#########################   Arbre de Décision     ##########################
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
unsmoted.model.fit <- rpart(connection_type ~ ., data = train.data)
## assessing
unsmoted.pred <- predict(unsmoted.model.fit, newdata = test.data, type = "class")
confusionMatrix(data = unsmoted.pred, reference = test.data$connection_type)
## ROC curve
unsmoted.probs <- predict(unsmoted.model.fit,newdata =  test.data, type = "prob")


unsmoted.roc <- roc(predictor = unsmoted.probs[,1], response = test.data$connection_type,
                    levels = rev(levels(test.data$connection_type)))

AUC = as.numeric(unsmoted.roc$auc)
plot(unsmoted.roc, main = "tree ROC", xlim = c(1,0), ylim = c(0,1), asp = NA)


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
custom.smoted.fit <- rpart( connection_type ~ ., data = custom.smote)
## assessing
custom.smoted.pred <- predict(custom.smoted.fit, newdata = test.data, type = "class")
confusionMatrix(data = custom.smoted.pred, reference = test.data$connection_type)
## ROC curve
custom.smoted.probs <- predict(custom.smoted.fit,newdata =  test.data,
                               type = "prob")
custom.smoted.roc <- roc(predictor = custom.smoted.probs[,1], response = test.data$connection_type,
                         levels = rev(levels(test.data$connection_type)))
AUC_SMOTE = as.numeric(custom.smoted.roc$auc) # Area under the curve : 0.9286
plot(custom.smoted.roc, main = "tree ROC", xlim = c(1,0), ylim = c(0,1), asp = NA, col ='blue')

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

custom.border.fit <- rpart(connection_type ~ ., data = custom.border)
## assessing
custom.border.pred <- predict(custom.border.fit, newdata = test.data2, type = "class")
confusionMatrix(data = custom.border.pred, reference = test.data$connection_type)
## ROC curve
custom.border.probs <- predict(custom.border.fit,newdata =  test.data,
                               type = "prob")
custom.border.roc <- roc(predictor = custom.border.probs[,1], response = test.data$connection_type,
                         levels = rev(levels(test.data$connection_type)))
AUC_BORDER = as.numeric(custom.border.roc$auc)
plot(custom.border.roc, main = "tree ROC", xlim = c(1,0), ylim = c(0,1), asp = NA, col='red')

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

custom.adasyn.fit <- rpart(connection_type ~ ., data = data.frame(custom.adasyn$data,connection_type = custom.adasyn$y))
## assessing
custom.adasyn.pred <- predict(custom.adasyn.fit, newdata = test.data, type = "class")
confusionMatrix(data = custom.adasyn.pred, reference = test.data$connection_type)
## ROC curve
custom.adasyn.probs <- predict(custom.adasyn.fit,newdata =  test.data,
                               type = "prob")
custom.adasyn.roc <- roc(predictor = custom.adasyn.probs[,1], response = test.data$connection_type,
                         levels = rev(levels(test.data$connection_type)))
AUC_ADASYN = as.numeric(custom.adasyn.roc$auc)
plot(custom.adasyn.roc, main = "tree ROC", xlim = c(1,0), ylim = c(0,1), asp = NA, col = 'green')


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

