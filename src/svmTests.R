
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


#### Importation de nos donnees. ####
 

data = read.table(file = "data/mydata.txt", sep=",")

data_val = data[3800:5000,]

data = data[1:3799,]


#### Visualisation du desequilibre des classes. ####

barplot(sort(table(data$connection_type), decreasing = TRUE))

prop.table(table(data$connection_type))



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
