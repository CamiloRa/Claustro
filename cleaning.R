#reading data in a DF, later to turn into list.

plants_df <- readLines("data/raw/plants.data.csv" )
head(plants_df)

str(plants_df[4])
