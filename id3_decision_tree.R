#############################################################################
####                  IMPORTING THE NECESSARY LIBRARIES                  ####
#############################################################################
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(igraph)
library(randomcoloR)

#############################################################################
####                       DEFINITION OF FUNCTIONS                       ####
#############################################################################

entropy <-function(label_col){
  
}

#############################################################################
####                            DATASETS LOAD                            ####
#############################################################################

# The first column in this example dataset corresponds to the number of the
# day, thus not useful for the computation
base_df <- read_csv("datasets/play_tennis.csv")[-1]

label_col <- dataset[]
