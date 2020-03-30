# Set wd  
setwd("/home/benjamin/Documents/credits_recherche")
# Libraries needed
library(igraph)

# Load the longformat matrix
# long_matrix <- readRDS("data/intermediate_object_results/matrix_inter_long.RDS")
wide_matrix <- t(readRDS("data/intermediate_object_results/matrix_inter_wide.RDS"))

# Frequency of each interaction
# matrix_link <- as.matrix(table(long_matrix$pred_scientific, long_matrix$prey_scientific, useNA = "always"))
# matrix_link <- spread(as.data.frame(table(long_matrix$pred_scientific, long_matrix$prey_scientific)))

g <- graph.adjacency(adjmatrix = wide_matrix, mode = "undirected", diag = TRUE, add.colnames = NULL)

#Graphique réseau complet
deg <- apply(wide_matrix, 2, sum, na.rm = TRUE)
rk <- rank(deg)
col.vec <- seq(10, 10, length.out = length(deg))
V(g)$size <- col.vec[rk]
col.base <- c("gold1","orange2","cyan4","darkblue")
col.vec <- colorRampPalette(col.base)(length(deg))
V(g)$color <- col.vec[rk]

layout <- layout_with_sugiyama(g, maxiter = 100)
layout <- layout.fruchterman.reingold(g, niter=7, circle = TRUE)
layout <- layout.reingold.tilford(g, circular=T)
plot(g, vertex.label= NA, vertex.size = 4, edge.arrow.mode=0, vertex.frame.color=NA, layout = layout_with_sugiyama(g), main = "Représentation du réseau entier")
quantile_deg <- quantile(deg,probs=c(0,0.33,0.66,1))
legend(x=-1.86,y=-1.4,legend=round(quantile_deg),col=col.base,pch=16,pt.cex=2.5,ncol=4,
       title="Nombre de liens")



