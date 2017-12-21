## Fichier: datasetDescription.r
## Auteur : Eric HAMMEL
## Description : Comparer les implémentations de smote
## Date : 17 décembre 2018

rm(list = ls())

library("DMwR")
library("caret")
library("microbenchmark")
source("src/smote.R", local = TRUE)
source("src/dataframeSetting.R", local = TRUE)

## import du jeu de données
kdd.data <- read.table("data/mydata.txt", header = TRUE, sep = ",")


trainIndex <- createDataPartition(kdd.data$connection_type, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
train.data <- kdd.data[trainIndex,]
dim(train.data)

## pre-smote plot
attack.freq <- table(kdd.data$connection_type)
print(attack.freq)
attack.prop <- prop.table(attack.freq)
print(attack.prop)
layout(matrix(1:2, nrow = 1, ncol = 2))
barplot(attack.freq, main = "occurrence des modalités")
barplot(attack.prop, main = "répartition des modalités")
layout(matrix(1, nrow = 1, ncol = 1))

## DMwR smote
dmwr.smote <- SMOTE(connection_type ~., data = train.data)
attack.freq <- table(dmwr.smote$connection_type)
print(attack.freq)
attack.prop <- prop.table(attack.freq)
print(attack.prop)
layout(matrix(1:2, nrow = 1, ncol = 2))
barplot(attack.freq, main = "occurrence des modalités")
barplot(attack.prop, main = "répartition des modalités")
layout(matrix(1, nrow = 1, ncol = 1))

## custom smote
custom.smote <- smote(target = "connection_type", data = train.data)
attack.freq <- table(custom.smote$connection_type)
print(attack.freq)
attack.prop <- prop.table(attack.freq)
print(attack.prop)
layout(matrix(1:2, nrow = 1, ncol = 2))
barplot(attack.freq, main = "occurrence des modalités")
barplot(attack.prop, main = "répartition des modalités")
layout(matrix(1, nrow = 1, ncol = 1))

## time execution comparison
## l'exécution des algorithme prends infiniment quand on les met dans 
## les fonctions microbenchmark ou system.time
bm <- microbenchmark(
  dmwr.smote <- SMOTE(connection_type ~., data = train.data),
  custom.smote <- smote(target = "connection_type", data = train.data))

print(bm)
ggplot2::autoplot(bm)
