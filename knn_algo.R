#Set the WD
setwd("/home/benjamin/Documents/credits_recherche") #Set my working directory

#libraries needed
library(vegan) #library vegan needed for vegdist()
library(plyr)
library(tidyverse)
library(purrr)
library(taxize)
rm(apg_families, apg_orders, theplantlist, rank_ref)

# Uplaod the interaction matrix
L <- read_rds("data/intermediate_object_results/matrix_inter_wide.RDS")
L_allQc <- read_rds("data/intermediate_object_results/matrix_inter_wide_all.RDS")

# Upload the distances matrix
D <- read_rds("data/intermediate_object_results/dist_mat_metanetwork.RDS")
D_allQc <- read_rds("data/intermediate_object_results/dist_mat_allQc.RDS")

#Find the k nearest-neighbors for each pred
knn_algo <- function(L, D, k){ # need to add the variable target somehow
pred_neighbor <- matrix(nrow = nrow(D), ncol = k) #Create an empty matrix to be filled
#each row of the matrix is a pred specie and the columns are its KNN
for(i in 1:nrow(pred_neighbor)){ #for loop that will go over each pred species
    count <- 0            #Initializing count, that will go up to the value of k
    pred_list <- NULL     #Initializing pred_list, that will be filled with the names of the KNN
    temp_pred <- D[i,]    #Initializing temp_pred, will take each pred by row with his distances
    if(sum(temp_pred, na.rm = TRUE) == 0){next}
    
      while(count < k){ #while loop that will count up to k
        nameMin <- names(which.min(temp_pred)) #nameMin will get the names of the KNN
        #nameMin <- names(which(temp_pred == min(temp_pred, na.rm =TRUE)))
        if(nameMin != rownames(D)[i]){ #If the name of the KNN is not the species that we are looking at
          pred_list <- append(pred_list, nameMin) #Store the name of the KNN
          temp_pred <- temp_pred[-which.min(temp_pred)] #remove the current KNN from the row
          count <- count + 1 #Add +1 to count and go to the next KNN
        }else{
          temp_pred <- temp_pred[-which.min(temp_pred)] #Remove the KNN from the row if it was the same specie
        }
      }
    pred_neighbor[i,] <- pred_list #Store the KNN of the pred specie in the matrix
}

#Subset L in length of number of species, to get 1 matrix per target species with their k-nearest-neighbor per list.
target_list <- list() #Initialize target_list, which will have lenght of the number of pred species
namesrow_L <- rownames(L) #Create vector of the names of all the pred species

for(i in 1:nrow(D)){ #for loop, that will iterate from 1 through the number of pred species
  target_list[[i]] <- matrix(L[i,], ncol = length(L[i,]), dimnames = list(namesrow_L[i])) #Will fill of the list, each element of the list is currently a matrix with a pred specie as row
  colnames(target_list[[i]]) <- colnames(L) #Will put the prey species as columns
}

for(i in 1:length(target_list)){ #for loop that will go through each element of the list
  temp_vect <- as.matrix(unlist(pred_neighbor[i,]), nrow = ncol(pred_neighbor[i,]), ncol = 1) #temp_vect get the name of the KNN 
  rownames(temp_vect) <- temp_vect[,1] #Put the name of the KNN as row names
  temp_vect <- temp_vect[,-1] #Remove their name from the 1st column
  temp_vect <- transform(merge(temp_vect, as.data.frame(L), by = 0, all = FALSE)) #Get the diet info of each KNN pred species
  rownames(temp_vect) <- temp_vect[,1] #Reset the row names
  temp_vect <- temp_vect[,-1] #Remove the first column again which contain the KNN names
  target_list[[i]] <- rbind(target_list[[i]], temp_vect) #Rbind the KNN to the matrix containing the pred species for which we want to predict its diet
}

#Compute the probability for each species
for(i in 1:length(target_list)){ #for loop, that will go through each element of the list
  target_sp <- target_list[[i]] #Get the i matrix of the list
  if(nrow(target_sp) < 2){next}
  
  unknown_inter <- which(is.na(target_sp[1,])) #Get each col number for which the interaction is NA
    for(j in unknown_inter){ #loop through the unknown interactions
      if(!all(is.na(target_sp[,j]))){prob_inter <- (sum(unlist(target_sp[,j]), na.rm = T))/(sum(!is.na(target_sp[,j])))} else{prob_inter <- NA} #if the column isn't all NA, we calculate the probability of interaction by summing the column of 0 and 1 (and omitting the NAs), and divide by the sum of 0 and 1 in the column. If the columns was all NA, we attribute NA to prob_inter.
      
      if(is.na(prob_inter)){target_sp[1,j] = NA} else{ 
        target_sp[1,j] = sample(c(0,1), size = 1, prob = c(1-prob_inter, prob_inter))} #If prob_inter is NA, we keep the NA as interaction, since we can't know. Else, if depending on the probability that is between 0 and 1, we will put 0 or 1 as the interaction. 
    }
  target_list[[i]] <- target_sp
}

infered_matrix <- do.call(rbind, (lapply(target_list, function(x) x[1,])))
#test <- matrix(infered_matrix)
#test <- do.call(rbind, (lapply(matrix(infered_matrix), function(x) x)))

return(infered_matrix)
}

system.time(infered_matrix_metanetwork <- knn_algo(L, D, k = 5))
system.time(infered_matrix_allQc <- knn_algo(L_allQc, D_allQc, k = 5))

write_rds(infered_matrix_metanetwork, "data/intermediate_object_results/infered_matrix_metanetwork")
write_rds(infered_matrix_allQc, "data/intermediate_object_results/infered_matrix_allQc")
rm(list=ls())

   