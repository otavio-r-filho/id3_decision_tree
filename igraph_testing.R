# Importing libraries
library(readr)
library(igraph)
library(randomcoloR)
library(dplyr)

# Loading the dataset
dataset <- read_csv("datasets/play_tennis.csv")[,-1]

# Creating vertice names and colors
v_names <- c(colnames(dataset[-5]), pull(unique(dataset[5])))
v_colors = replicate(length(v_names), randomColor())
names(v_colors) <- v_names
v_colors["yes"] <- "green"
v_colors["no"] <- "red"

# Creates an undirected graph
g <- graph.empty(directed = F)

v_name = "weather"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, color = v_color, attr = list())      #v1

v_name = "humidity"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, color = v_color, attr = list())      #v2

g <- add_edges(g, c(1,2), label = "sunny", attr = list())    #v1--v2

v_name = "wind"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, color = v_color, attr = list())      #v3

g <- add_edges(g, c(1,3), label = "rain", attr = list())    #v1--v3

v_name = "yes"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, color = v_color, attr = list())      #v4

g <- add_edges(g, c(1,4), label = "cloudy", attr = list())    #v1--v4

v_name = "no"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, color = v_color, attr = list())      #v5

g <- add_edges(g, c(2,5), label = "high", attr = list())    #v2--v5

v_name = "yes"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, color = v_color, attr = list())      #v6

g <- add_edges(g, c(2,6), label = "normal", attr = list())    #v2--v6

v_name = "no"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, color = v_color, attr = list())      #v7

g <- add_edges(g, c(3,7), label = "strong", attr = list())    #v3--v7

v_name = "yes"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, color = v_color, attr = list())      #v8

g <- add_edges(g, c(3,8), label = "weak", attr = list())    #v3--v8

plot(g, layout = layout_as_tree)
