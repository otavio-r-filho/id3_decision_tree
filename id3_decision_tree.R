#############################################################################
####                  IMPORTING THE NECESSARY LIBRARIES                  ####
#############################################################################
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(igraph)

#############################################################################
####                       DEFINITION OF FUNCTIONS                       ####
#############################################################################

# Function to generate random colors
randomColor <- function(){
  new_color <- round(runif(3, min = 0, max = 255), digits = 0)
  new_color <- as.character.hexmode(new_color)
  paste("#", new_color[1], new_color[2], new_color[3], sep = "")
}

entropy <-function(label_col){
  probs_den <- nrow(label_col)
  probs <- as.vector(table(label_col)) / probs_den
  -sum(probs * log2(probs))
}

decouple_col <- function(ds, col_name, col_val, label_col) {
  filter_ds <- ds %>% filter(col_name == col_val)
  return(filter_ds[label_col])
}

information_gain <- function(df, col_name, label_col){
  
}

#############################################################################
####                            DATASETS LOAD                            ####
#############################################################################

# The first column in this example dataset corresponds to the number of the
# day, thus not useful for the computation
base_df <- read_csv("datasets/play_tennis.csv")[-1]

#############################################################################
####                            TESTING AREA                             ####
#############################################################################

df <- base_df
col_name <- "wind"
col_val <- "strong"
label_col <- "play_tennis"
probs_den <- nrow(df)
probs <- as.vector(table(df[col_name])) / probs_den
col_names <- names(table(df[col_name]))
filter_ds <- apply(ds, filter_col, ...)

unq_vals <- unique(base_df[col_name])
for(condition in unique(base_df[col_name])){
  print(un)
}
