## Fichier: datasetDescription.r
## Auteur : Eric HAMMEL
## Description : Description du jeu de données
## Date : 17 décembre 2018

rm(list = ls())

# importing dataset
source("src/dataframeSetting.R", local = TRUE)
kdd.data <- createDataset()
test.data <- kdd.data
# dataset dimension
dim(kdd.data)
# Observations : 4898431
# variables : 41 (la 42ème est la variable à prédire)

# class répartition
attack.freq <- table(kdd.data$attack_type)
attack.prop <- prop.table(attack.freq)
layout(matrix(1:4, nrow = 2, ncol = 2))
barplot(head(sort(attack.freq, decreasing = TRUE)), main = "attaques les plus fréquentes en occurence")
barplot(tail(sort(attack.freq, decreasing = TRUE)), main = "attaques les moins fréquentes en occurenc")
barplot(head(sort(attack.prop, decreasing = TRUE)), main = "réparition des attaques les plus fréquentes")
barplot(head(sort(attack.prop, decreasing = TRUE)), main = "réparition des attaques les moins fréquentes")
layout(matrix(1, nrow = 1, ncol = 1))

#### Binarisation ####

## On peut essayer de recoder les types d'attaques, on garde la modilité normales
## et les reste on l'appel attacked

levels(test.data$attack_type)[levels(test.data$attack_type) != "normal"] <- "attacked"
attack.freq <- table(test.data$attack_type)
attack.prop <- prop.table(attack.freq)
layout(matrix(1:4, nrow = 2, ncol = 2))
barplot(head(sort(attack.freq, decreasing = TRUE)), main = "attaques les plus fréquentes en occurence")
barplot(tail(sort(attack.freq, decreasing = TRUE)), main = "attaques les moins fréquentes en occurenc")
barplot(head(sort(attack.prop, decreasing = TRUE)), main = "réparition des attaques les plus fréquentes")
barplot(head(sort(attack.prop, decreasing = TRUE)), main = "réparition des attaques les moins fréquentes")
layout(matrix(1, nrow = 1, ncol = 1))
