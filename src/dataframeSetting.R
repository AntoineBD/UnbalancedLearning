## Fichier: dataframeSetting.r
## Auteur : Eric HAMMEL
## Description : Importation du jeu de données et 
##               modifications nécessaires pour les diverses manipulations
## Date : 17 décembre 2018

## Importation du jeu de données kddcup en utilisant la librairie data.table qui 
## est adpatée à un fichier de cette taille
## INPUT : pas d'INPUT
## OUTPUT :
##  le jeu de données de type data.table
createDataset <- function()
{
  library("data.table") # library adapting dataframes for large datasets
  # setting up header
  dtHeader <- read.table("data/kddcup.header.txt", sep = ",", header = TRUE)
  
  # loading dataset without the header
  kddcup.dataset <- fread(input = "data/kddcup.data.corrected", sep = ",",
                          header = FALSE, col.names = names(dtHeader),
                          stringsAsFactors = TRUE)
  # removing the point from attack_type factor
  kddcup.dataset$attack_type <- as.factor(gsub(".$","",kddcup.dataset$attack_type))
  return(kddcup.dataset)
  }


