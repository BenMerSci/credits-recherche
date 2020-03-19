# Scripts that will calculate the distance matrices (D), needed as a input for the knn_algo
setwd("/home/benjamin/Documents/credits_recherche")
library(plyr)
library(tidyverse)
library(vegan)
#########################################################################################################
######################### Distance matrix for the base metanetwork system ###############################
#########################################################################################################

#Distance matrix for the simple metanetwork
L <- read_rds("data/intermediate_object_results/matrix_inter_wide.RDS")#Read the matrix
pred_phylogeny <- read_rds("data/intermediate_object_results/pred_phylogeny.RDS")

# In pred_phylogeny, replace the species that don't have any phylogenetic info, with a blank matrix filled with NAs so that I can correctly rbind each of them
for(i in as.numeric(which(is.na(pred_phylogeny)))){
  pred_phylogeny[[i]] <- as.data.frame(matrix(rep(NA, 2*nrow(pred_phylogeny[[1]])), ncol = 3, nrow = 7))
}
# rbind all the phylogenic info of each species together, to get the right input for vegan::taxa2dist
pred_phylogeny <- do.call(rbind.fill, lapply(pred_phylogeny, function(x) as.data.frame(matrix(x[,1], nrow = 1))))

# Calculte the phylogenetic distances, put it between [0,1]
Dist_mat_phylo <- as.matrix((taxa2dist(pred_phylogeny))/100) 
# Calculate the interaction distances, that are already between [0,1]
Dist_mat_inter <- as.matrix(vegdist(L, method = "jaccard", na.rm = TRUE))

D <- matrix(nrow = nrow(L), ncol = ncol(L))
# Put the two distances together
for(i in 1:nrow(L)){
  if(sum(is.na(L[i,])) == ncol(L)) {D[i,] <- ((0*Dist_mat_inter[i,])+(1*Dist_mat_phylo[i,]))}
  else{D[i,] <- ((0.8*Dist_mat_inter[i,])+(0.2*Dist_mat_phylo[i,]))}
}

rownames(D) <- rownames(L)
colnames(D) <- colnames(L)
rm(list=setdiff(ls(), c("pred_phylogeny","D")))

#########################################################################################################
######################### Distance matrix for the full QC system ########################################
#########################################################################################################

# Distance matrix for the meta-network + the qc species that were not in it
L_allQc <- read_rds("data/intermediate_object_results/matrix_inter_wide_all.RDS")
pred_phylogeny_miss <- read_rds("data/intermediate_object_results/pred_phylogeny_miss.RDS") 

# In pred_phylogeny, replace the species that don't have any phylogenetic info, with a blank matrix filled with NAs so that I can correctly rbind each of them
for(i in as.numeric(which(is.na(pred_phylogeny_miss)))){
  pred_phylogeny_miss[[i]] <- as.data.frame(matrix(rep(NA, 2*nrow(pred_phylogeny_miss[[1]])), ncol = 3, nrow = 7))
}

# rbind all the phylogenic info of each species together, to get the right input for vegan::taxa2dist
pred_phylogeny_miss <- do.call(rbind.fill, lapply(pred_phylogeny_miss, function(x) as.data.frame(matrix(x[,1], nrow = 1))))
pred_phylogeny_miss <- rbind(pred_phylogeny, pred_phylogeny_miss)

# Calculte the phylogenetic distances, put it between [0,1]
Dist_mat_phylo_allQc <- as.matrix((taxa2dist(pred_phylogeny_miss))/100) 
# Calculate the interaction distances, that are already between [0,1]
Dist_mat_inter_allQc <- as.matrix(vegdist(L_allQc, method = "jaccard", na.rm = TRUE))

# positions of the matrix with NA
indexNA <- which(is.na(L_allQc))
# Matrix of result (empty with NA)
D_allQc <- matrix(nrow = nrow(L_allQc), ncol = ncol(L_allQc))
# When mInter == NA: (0 * matrice dist_interaction) + (1 * matrice_dist_phylogénie)
D_allQc[indexNA] <- Dist_mat_phylo_allQc[indexNA]
# When mInter != NA: (0,8 * matrice dist_interaction) + (0,2 * matrice dist_phylogénie)
D_allQc[-indexNA] <- mapply(sum, (Dist_mat_inter_allQc[-indexNA] * 0.8), (Dist_mat_phylo_allQc[-indexNA] * 0.2), na.rm = TRUE)


rownames(D_allQc) <- rownames(L_allQc)
colnames(D_allQc) <- colnames(L_allQc)

rm(list=setdiff(ls(), c("D","D_allQc")))

write_rds(D, "data/intermediate_object_results/dist_mat_metanetwork.RDS")
write_rds(D_allQc, "data/intermediate_object_results/dist_mat_allQc.RDS")

rm(list=ls())






##############################################################################################################################################################
############################ TESTING SECTION TO MAKE THE DISTANCE MATRICES WITH WEIGHT #######################################################################

#Dist_list_allQc <- list(Dist_mat_phylo_allQc, Dist_mat_inter_allQc)
# 
# D_allQc <- matrix(nrow = nrow(L_allQc), ncol = ncol(L_allQc))
# L_allQcNA <- is.na(L_allQc)
#   
# for(i in 1:nrow(L_allQc)){
#   for(j in 1:ncol(L_allQc)){
#     if(is.na(L_allQc[i,j]) {D_allQc[i,j] <- }
#   }
# }
# 
# 
# 
# 
# for(i in 1:nrow(L_allQc)){
# D_allQc[i,] <- lapply(nrow(L_allQc), function(x) if(all(is.na(L_allQc[x,])) == TRUE) {lapply(ncol(D_allQc), function(sum(x))} else {sum(x)}))
# }
# test <- apply(simplify2array(Dist_list_allQc), 1:2, function(x) if(all(is.na(x)) == TRUE) {sum(x, is.na = TRUE)} else sum(x, na.rm = TRUE))
# test <- apply(simplify2array(Dist_list_allQc), 1:2, function(x) sum(c(na.omit(x), NA[1])))
#   
# if(sum(is.na(L_allQc[i,])) == ncol(L_allQc)) {D_allQc[i,] <- (sum((0*Dist_mat_inter_allQc[i,]), (1*Dist_mat_phylo_allQc[i,]), na.rm = TRUE))} else 
# {D_allQc[i,] <- ((0.8*Dist_mat_inter_allQc[i,])+(0.2*Dist_mat_phylo_allQc[i,]))}
# 
# na.m_inter <- is.na(Dist_mat_inter_allQc)
# na.m_phylo <- is.na(Dist_mat_phylo_allQc)
# test <- ifelse(na.m_inter & na.m_phylo, NA, ifelse(na.m_inter, 0, Dist_mat_inter_allQc) + ifelse(na.m_phylo, 0, Dist_mat_phylo_allQc))
# 
# for(i in 1:nrow(L)){
#   if(sum(is.na(L[i,])) == ncol(L)) {D[i,] <- ((0*Dist_mat_inter[i,])+(1*Dist_mat_phylo[i,]))}
#   else{D[i,] <- ((0.8*Dist_mat_inter[i,])+(0.2*Dist_mat_phylo[i,]))}
# }

