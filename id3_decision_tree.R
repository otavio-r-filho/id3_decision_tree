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

argmax <- function(df){
  which.max(df)
}

entropy <-function(label_col){
  probs_den <- nrow(label_col)
  probs <- as.vector(table(label_col)) / probs_den
  -sum(probs * log2(probs))
}

filter_df <- function(col_val, col_name, label_col, df) {
  filtered_df <- df[which(df[col_name] == col_val),]
  filtered_df <- filtered_df[label_col]
}

information_gain <- function(col_name, label_col, df){
  df_E <- entropy(df[label_col])
  probs_den <- nrow(df)
  probs <- as.vector(table(df[col_name])) / probs_den
  col_vals <- names(table(df[col_name]))
  filtered_df <- lapply(col_vals, filter_df, col_name = col_name, label_col = label_col, df = df)
  col_E <- unlist(lapply(filtered_df, entropy))
  df_E - sum(probs * col_E)
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

label_col <- "play_tennis"
df <- base_df

col_names <- names(df[, -which(names(df) == label_col)])
igains <- lapply(col_names, information_gain, label_col = label_col, df = df)

vertex_fifo <- c()
if(any(igains > 0.0)){
  append(vertex_fifo, col_names[argmax(igains)])
}
