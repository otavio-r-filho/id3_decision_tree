# Importing libraries
library(readr)
library(igraph)
library(randomcoloR)

# Auxiliary functions
# Function to count unique values
count_unique <- function(dataset_column){
  return(length(unique(dataset_column)))
}

# Loading the dataset
dataset <- read_csv("datasets/play_tennis.csv")[,-1]

# Calculating the maximum number of child nodes in the tree
max_children = max(apply(dataset, 2, count_unique))
v_colors = list()

# Creates an undirected graph
g <- graph.empty(directed = F)

label = "weater"
if(is.null(v_colors[label])){v_colors[label] = randomColor()}
g <- add_vertices(g, 1, label = label, color = v_colors[label], attr = list())      #v1

label = "umidity"
if(is.null(v_colors[label])){v_colors[label] = randomColor()}
g <- add_vertices(g, 1, label = label, color = v_colors[label], attr = list())      #v2

g <- add_edges(g, c(1,2), label = "sunny", attr = list())    #v1--v2

label = "wind"
if(is.null(v_colors[label])){v_colors[label] = randomColor()}
g <- add_vertices(g, 1, label = label, color = v_colors[label], attr = list())      #v3

g <- add_edges(g, c(1,3), label = "rain", attr = list())    #v1--v3

label = "yes"
if(is.null(v_colors[label])){v_colors[label] = randomColor()}
g <- add_vertices(g, 1, label = label, color = v_colors[label], attr = list())      #v4

g <- add_edges(g, c(1,4), label = "cloudy", attr = list())    #v1--v4

label = "no"
if(is.null(v_colors[label])){v_colors[label] = randomColor()}
g <- add_vertices(g, 1, label = label, color = v_colors[label], attr = list())      #v5

g <- add_edges(g, c(2,5), label = "high", attr = list())    #v2--v5

label = "yes"
if(is.null(v_colors[label])){v_colors[label] = randomColor()}
g <- add_vertices(g, 1, label = label, color = v_colors[label], attr = list())      #v6

g <- add_edges(g, c(2,6), label = "normal", attr = list())    #v2--v6

label = "no"
if(is.null(v_colors[label])){v_colors[label] = randomColor()}
g <- add_vertices(g, 1, label = label, color = v_colors[label], attr = list())      #v7

g <- add_edges(g, c(3,7), label = "strong", attr = list())    #v3--v7

label = "yes"
if(is.null(v_colors[label])){v_colors[label] = randomColor()}
g <- add_vertices(g, 1, label = label, color = v_colors[label], attr = list())      #v8

g <- add_edges(g, c(3,8), label = "weak", attr = list())    #v3--v8

plot(g, layout = layout_as_tree)
