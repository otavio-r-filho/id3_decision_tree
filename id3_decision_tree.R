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

# Function to calculate the entropy
entropy <-function(label_col){
  probs_den <- nrow(label_col)
  probs <- as.vector(table(label_col)) / probs_den
  -sum(probs * log2(probs))
}

# Function to get filtered labels column
filter_df <- function(col_val, col_name, label_col, df) {
  filtered_df <- df[which(df[col_name] == col_val),]
  filtered_df <- filtered_df[label_col]
}

# Function to filter multiple columns with multiple values
filter_cols <- function(col_names, col_vals, df){
  filtered_df <- df
  for(i in seq(length(col_names))){
    filtered_df <- filtered_df[which(filtered_df[col_names[i]] == col_vals[i]),]
  }
  return(filtered_df)
}

get_vertice_names <- function(df, label_col){
  v_names <- names(df[, -which(names(df) == label_col)])
  return(v_names)
}

# Function the get the color of the nodes
get_vertice_colors <- function(df, label_col){
  v_names = c(get_vertice_names(df, label_col), unname(unlist(unique(df[label_col]))))
  v_colors = replicate(length(v_names), randomColor())
  names(v_colors) = v_names
  return(v_colors)
}

# Function to calculate the information gain
information_gain <- function(col_name, label_col, df){
  df_E <- entropy(df[label_col])
  probs_den <- nrow(df)
  probs <- as.vector(table(df[col_name])) / probs_den
  col_vals <- names(table(df[col_name]))
  filtered_df <- lapply(col_vals, filter_df, col_name = col_name, label_col = label_col, df = df)
  col_E <- unlist(lapply(filtered_df, entropy))
  df_E - sum(probs * col_E)
}

id3_decision_tree <- function(df, label_col) {
  
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

id3_tree <- graph.empty(directed = FALSE)
v_names <- get_vertice_names(df, label_col)
tree_root <- names(argmax(sapply(v_names, information_gain, label_col = label_col, df = df)))
father_q <- tree_root
v_father <- NULL
v_child <- NULL
while(length(father_q) > 0){
  v_father <- father_q[1]
  edges <- unlist(unique(df[v_father]))
  print(c(v_father, edges))
  for(e in edges){
    
  }
  v_names <- v_names[!v_names %in% v_father]
}

id3_tree <- graph.empty(directed = FALSE)
id3_tree <- add_vertices(id3_tree, 1, name = "weather", label = "weather", attr = list())
id3_tree <- add_vertices(id3_tree, 1, name = "humidity", label = "humidity", attr = list())
id3_tree <- add_vertices(id3_tree, 1, name = "no1", label = "no", attr(leaf_label = "no"))
id3_tree <- add_vertices(id3_tree, 1, name = "yes1", label = "yes", attr(leaf_label = "yes"))
id3_tree <- add_edges(id3_tree, c("weather","humidity"), label = "sunny", name = "sunny", attr = list())
plot(id3_tree, layout = layout_as_tree)
