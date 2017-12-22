## Fichier: borderLineSmoteComparison.R
## Auteur : Ilham MOUZOURI
## Description : Comparer les implémentations de border line smote
## Date : 21 décembre 2018

rm(list = ls())

library(smotefamily)
library(FNN)
library("caret")
library("microbenchmark")
source("src/borderLineSmote.R", local = TRUE)
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

## data transofrmation
train.data$connection_type = as.character(train.data$connection_type)
train.data$connection_type[train.data$connection_type == 'good'] = 1
train.data$connection_type[train.data$connection_type == 'bad'] = 0
train.data$connection_type = as.numeric(train.data$connection_type)

## DMwR smote
smotefamily.borderLineSmote <- BLSMOTE(train.data, train.data$connection_type, K = 3)
attack.freq <- table(smotefamily.borderLineSmote$data$connection_type)
print(attack.freq)
attack.prop <- prop.table(attack.freq)
print(attack.prop)
layout(matrix(1:2, nrow = 1, ncol = 2))
barplot(attack.freq, main = "occurrence des modalités")
barplot(attack.prop, main = "répartition des modalités")
layout(matrix(1, nrow = 1, ncol = 1))

## custom smote
custom.borderLineSmote <- BorderLineSMOTE1(train.data, train.data$connection_type, K = 3)
attack.freq <- table(custom.borderLineSmote$connection_type)
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
  smotefamily.borderLineSmote <- BLSMOTE(train.data, train.data$connection_type, K = 3),
  custom.borderLineSmote <- BorderLineSMOTE1(train.data, train.data$connection_type, K = 3))

print(bm)
ggplot2::autoplot(bm)
