# Importing libraries
library(readr)
library(igraph)
library(dplyr)

# Loading the dataset
dataset <- read_csv("datasets/play_tennis.csv")[,-1]

randomColor <- function(){
  new_color <- round(runif(3, min = 0, max = 255), digits = 0)
  new_color <- as.character.hexmode(new_color)
  paste("#", new_color[1], new_color[2], new_color[3], sep = "")
}

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
g <- add_vertices(g, 1, label = v_name, name = v_name, color = v_color, attr = list())      #v1

v_name = "humidity"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, name = v_name, color = v_color, attr = list())      #v2

g <- add_edges(g, c("weather", "humidity"), label = "sunny", name = "sunny", attr = list())    #v1--v2

v_name = "wind"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, name = v_name, color = v_color, attr = list())      #v3

g <- add_edges(g, c("weather","wind"), label = "rain", name = "rain",attr = list())    #v1--v3

v_name = "yes"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, name = "yes1", color = v_color, attr = list())      #v4

g <- add_edges(g, c("weather","yes1"), label = "cloudy", name = "cloudy", attr = list())    #v1--v4

v_name = "no"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, name = "no1",color = v_color, attr = list())      #v5

g <- add_edges(g, c("humidity","no1"), label = "high", name = "high", attr = list())    #v2--v5

v_name = "yes"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, name = "yes2", color = v_color, attr = list())      #v6

g <- add_edges(g, c("humidity","yes2"), label = "normal", name = "normal", attr = list())    #v2--v6

v_name = "no"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, name = "no2", color = v_color, attr = list())      #v7

g <- add_edges(g, c("wind","no2"), label = "strong", name = "strong", attr = list())    #v3--v7

v_name = "yes"
v_color = unname(v_colors[v_name])
g <- add_vertices(g, 1, label = v_name, name = "yes3",color = v_color, attr = list())      #v8

g <- add_edges(g, c("wind","yes3"), label = "weak", name = "weak", attr = list())    #v3--v8

plot(g, layout = layout_as_tree(g, root = "weather"))

spath <- names(unlist(shortest_paths(g, from = "weather", "humidity", output = "both")$vpath))
spath <- spath[-which(spath == "humidity")]
sedge <- names(unlist(shortest_paths(g, from = "weather", "humidity", output = "both")$epath))

spath
sedge
