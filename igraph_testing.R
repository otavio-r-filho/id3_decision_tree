# Importing libraries
library(readr)
library(igraph)

# Auxiliary functions
# Function to count unique values
count_unique <- function(dataset){
  
}

# Loading the dataset
dataset <- read_csv("datasets/play_tennis.csv")[,-1]

max_children <- unique(dataset)
