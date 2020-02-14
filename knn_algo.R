#Set the WD
setwd("/home/benjamin/Documents/credits_recherche")

#libraries needed
library(vegan)
library(tidyverse)
#Uplaod the matrix
L <- read_rds("R/matrix_inter.RDS") %>%
  t(.)

#distance matrix
D_matrix <- vegdist(L, method = "jaccard", na.rm = TRUE) 
D <- as.matrix(D_matrix)

#Find the k nearest-neighbors for each pred
#knn_algo <- function(L, D, target, k){}

k <- 10
pred_neighbor <- as.data.frame(matrix(nrow = nrow(D), ncol = k))
for(i in 1:nrow(pred_neighbor)){
    count <- 0            #initializing variables
    pred_list <- NULL
    temp_pred <- D[i,]
    
      while(count < k){
        nameMin <- names(which.min(temp_pred))
        if(nameMin != rownames(D)[i]){
          pred_list <- append(pred_list, nameMin)
          temp_pred <- temp_pred[-which.min(temp_pred)]
          count <- count + 1
        }else{
          temp_pred <- temp_pred[-which.min(temp_pred)]
        }
      }
    pred_neighbor[i,] <- pred_list
}

#Subset L in length of number of species, to get 1 matrix per target species with their k-nearest-neighbor per list.
target_list <- list()
namesrow_L <- rownames(L)

for(i in 1:nrow(D)){
  target_list[[i]] <- matrix(L[i,], ncol = length(L[i,]), dimnames = list(namesrow_L[i]))
  colnames(target_list[[i]]) <- colnames(L)
}

for(i in 1:length(target_list)){
  temp_vect <- as.data.frame(as.matrix(unlist(pred_neighbor[i,]), nrow = ncol(pred_neighbor[i,]), ncol = 1))
  rownames(temp_vect) <- temp_vect[,1] 
  temp_vect <- temp_vect[,-1]
  temp_vect <- transform(merge(temp_vect, as.data.frame(L), by = 0, all = FALSE))
  rownames(temp_vect) <- temp_vect[,1]
  temp_vect <- temp_vect[,-1]
  target_list[[i]] <- rbind(target_list[[i]], temp_vect)
}

#Compute the probability for each species
#get the first row, which(all()) pour la première
for(i in length(target_list)){
  target_sp <- target_list[[i]]
  unknown_inter <- which(is.na(target_sp[1,]))
    if(unknown_inter){}
}

which(is.na(test[1,]))
which(all(is.na(x)))