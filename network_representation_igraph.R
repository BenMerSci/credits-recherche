library(igraph)
test <- table(L)
g <- graph.adjacency(adjmatrix = test, mode = "undirected", add.colnames = NA)