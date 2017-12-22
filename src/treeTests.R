
############################################################################
############################################################################
#########################   Arbre de Décision     ##########################
############################################################################
############################################################################

### Quelques library
library(smotefamily)
library(KernelKnn)
library(tree)
library(FNN)

## Custom borderLineSmote
source("src/borderLineSmote.R", local = TRUE)

#### Importation de nos donnees. ####
data = read.table(file = "data/mydata.txt", sep=",")
data_val = data[3800:5000,]
data = data[1:3799,]

#### Visualisation du desequilibre des classes. ####
# barplot(sort(table(data$connection_type), decreasing = TRUE))
# prop.table(table(data$connection_type))

########################## MODELE TREE/PREDICTIONS ##########################

### Modification de nos donnees, variable a prédire en 'bad'/'good'.
data$connection_type = as.character(data$connection_type)
data$connection_type[data$connection_type == 'good'] = 1
data$connection_type[data$connection_type == 'bad'] = 0
data$connection_type = as.numeric(data$connection_type)


################# Application de l'arbre de decision  #######################
arbre <- tree(connection_type~., data)
Pred = predict(arbre, data[,-ncol(data)])

######################## orderlineSMOTE  ####################################

result <- BorderLineSMOTE1(data, data$connection_type, K=3)




