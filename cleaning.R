#reading data in a DF, later to turn into categories
#load libraries
library(dplyr)
library(purrr)

#load functions
readfirstcolumn <- function  (dataframe) {
  dataframe[1,1] %>%
  return(nrow (dataframe))
  
  
}

#read data
plants_df <- readLines("data/raw/plants.data.csv" ) %>%
  as.data.frame(stringsAsFactors = FALSE)



#make a column
readfirstcolumn(plants_df)

  


str(plants_df[4,])
plants_df[4:8,]
colnames(plants_df)
colnames(plants_df) <- "plant_name"
dim(plants_df)
