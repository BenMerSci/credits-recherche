# Libraries needed
library(igraph)

# Load the longformat matrix
long_matrix <- readRDS("data/intermediate_object_results/matrix_inter_long.RDS")
wide_matrix <- readRDS("data/intermediate_object_results/matrix_inter_wide.RDS")

# Frequency of each interaction
matrix_link <- spread(as.data.frame(table(long_matrix$pred_scientific, long_matrix$prey_scientific)))

g <- graph.adjacency(adjmatrix = matrix_link, mode = "undirected", add.colnames = NA)
