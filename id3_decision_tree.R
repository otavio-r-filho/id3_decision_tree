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

gain_ratio <- function(col_name, label_col, df){
  df_E <- entropy(df[label_col])
  igain <- information_gain(col_name, label_col, df)
  return(igain/df_E)
}

col_name <- "wind"
label_col <- "play_tennis"
df <- base_df

gindex(col_name = col_name, df = df)
gindex(col_name = label_col, df = df)

gindex <- function(col_name, df){
  probs <- as.vector(table(df[col_name])) / nrow(df)
  return(1 - sum(probs^2))
}

gini_index <- function(col_name, label_col, df){
  IS <- gindex(label_col, df)
  probs <- as.vector(table(df[col_name])) / nrow(df)
  gi <- 1.0 - sum(probs^2)
}

id3_decision_tree <- function(df, label_col) {
  id3_tree <- graph.empty(directed = FALSE)
  
  leafs_names <- unlist(unique(df[label_col]))
  leafs_count <- rep(0, length(leafs_names))
  names(leafs_count) <- leafs_names
  
  v_names <- get_vertice_names(df, label_col)
  tree_root <- names(argmax(sapply(v_names, information_gain, label_col = label_col, df = df)))
  father_q <- tree_root
  v_names <- v_names[-which(v_names == tree_root)]
  
  v_colors <- get_vertice_colors(df, label_col)
  v_color = unname(v_colors[tree_root])
  id3_tree <- add_vertices(id3_tree, 1, label = tree_root, name = tree_root, color = v_color, attr = list())
  
  v_father <- NULL
  v_child <- NULL
  
  while(length(father_q) > 0){
    v_father <- father_q[1]
    father_q <- father_q[-1]
    
    if(v_father == tree_root){
      v_filtered_df <- df
    } else {
      spath <- shortest_paths(id3_tree, from = tree_root, to = v_father, output = "both")
      vpath <- names(unlist(spath$vpath))
      vpath <- vpath[-which(vpath == v_father)]
      epath <- names(unlist(spath$epath))
      v_filtered_df <- filter_cols(vpath, epath, df)
    }
    
    edges <- unname(unlist(unique(v_filtered_df[v_father])))
    for(e in edges){
      e_filtered_df <- filter_cols(v_father, e, v_filtered_df)
      igains = sapply(v_names, information_gain, label_col = label_col, df = e_filtered_df)
      
      if(max(igains) == 0) { 
        leaf_label <- unname(unlist(unique(e_filtered_df[label_col])))
        leafs_count[leaf_label] = leafs_count[leaf_label] + 1
        v_color = unname(v_colors[leaf_label])
        leaf_name <- paste(leaf_label, leafs_count[leaf_label], sep = "")
        id3_tree <- add_vertices(id3_tree, 1, label = leaf_label, name = leaf_name, color = v_color, attr = list())
        id3_tree <- add_edges(id3_tree, c(v_father, leaf_name), label = e, name = e, attr = list())
      } else {
        v_name <- names(argmax(igains))
        v_color = unname(v_colors[v_name])
        id3_tree <- add_vertices(id3_tree, 1, label = v_name, name = v_name, color = v_color, attr = list())
        id3_tree <- add_edges(id3_tree, c(v_father, v_name), label = e, name = e, attr = list())
        igains <- sapply(v_names, information_gain, label_col = label_col, df = e_filtered_df)
        father_q <- append(father_q, names(argmax(igains)))
        v_names <- v_names[-which(v_names == v_name)]
      }
    }
  }
  
  return(list(
    tree = id3_tree,
    root = tree_root
  ))
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

id3_tree <- id3_decision_tree(df = base_df, label_col = label_col)
plot(id3_tree$tree, layout = layout_as_tree(id3_tree$tree, root = id3_tree$root))
