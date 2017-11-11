
library("data.table") # library adapting dataframes for large datasets

createDataset <- function()
{
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
