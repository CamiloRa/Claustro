#reading data in a DF, later to turn into categories
#load libraries
library(dplyr)

#load functions
readfirstcolumn <- function  (dataframe) {
  return(nrow (dataframe))
}

#read data
plants <- readLines("data/raw/plants.data.csv" )
plants_df <- as.data.frame(plants)

#make a column
readfirstcolumn(plants_df)

  


str(plants_df[4,]
plants_df[4:8,]
colnames(plants_df)
colnames(plants_df) <- "plant_name"
dim(plants_df)
