#libraries
library(stringr)
library(taxize)
library(tidyverse)
library(vegan)
rm(apg_families, apg_orders, rank_ref, theplantlist)
#set the WD
setwd("/home/benjamin/Documents/credits_recherche/")

#read the data from every network
cohen_web23 <- read.csv("data/Cohen_ecoweb/web23.csv", header = T)
cohen_web24 <- read.csv("data/Cohen_ecoweb/web24.csv", header = T)
cohen_web25 <- read.csv("data/Cohen_ecoweb/web25.csv", header = T)
cohen_web26 <- read.csv("data/Cohen_ecoweb/web26.csv", header = T)
cohen_web59 <- read.csv("data/Cohen_ecoweb/web59.csv", header = T)
legagneux_2014 <- read.csv("data/legagneux_2014/interactions_alert_bylot.csv", header = T)
leroux_2014 <- read.csv("data/leroux_2014/leroux_2014.csv", header = T)
msh_2010 <- read.csv("data/MSH_2010/MSH_food_web_original/MSH_food web_original/1_Working files/interactions_msh.csv", header = T)
ropars_2018 <- read.csv("data/Ropars_nordic_2018/ds_000582043/interactions_nunavik.csv", header = T)
  
#Combining them into one dataframe
meta_network <- rbind(cohen_web23, cohen_web24, cohen_web25, cohen_web26, cohen_web59, legagneux_2014, leroux_2014, msh_2010, ropars_2018)

#rmeoving duplicate interaction
meta_network$pred_scientific <- tolower(meta_network$pred_scientific)
meta_network$prey_scientific <- tolower(meta_network$prey_scientific)
meta_network <- meta_network[!duplicated(meta_network[,c('pred_scientific','prey_scientific')]),]
meta_network$pred_scientific <-  paste0(str_to_upper(str_extract(meta_network$pred_scientific, ".{1}")), str_remove(meta_network$pred_scientific, ".{1}"))
meta_network$prey_scientific <-  paste0(str_to_upper(str_extract(meta_network$prey_scientific, ".{1}")), str_remove(meta_network$prey_scientific, ".{1}"))

#write it to .csv
write.csv(meta_network, "R/meta_network_long.csv", row.names = F)
meta_network <- read.csv("R/meta_network_long.csv", header = T)

#Checking the number of unique pred in the system, to see how many columns I should have
#Final correction for taxo too, to see if we have duplicate but written different
meta_network <- meta_network[,c("pred_scientific","prey_scientific","interaction")]
pred_char <- unique(as.character(meta_network$pred_scientific))
corrected_taxo_all_pred <- gnr_resolve(pred_char, best_match_only = T, canonical = T)
which(corrected_taxo_all_pred$submitted_name != corrected_taxo_all_pred$matched_name2)
corrected_taxo_all_pred[63,"matched_name2"] <- "Felis catus"
unique_pred <- unique(as.character(corrected_taxo_all_pred$matched_name2))

#put the matrix from long to wide
L <- meta_network
L$pred_scientific <- as.character(L$pred_scientific)
L$prey_scientific <- as.character(L$prey_scientific)
L <- spread(meta_network, pred_scientific, interaction)
rownames(L) <- L$prey_scientific
L <- L[,-1]

pred_name <- colnames(L)
correct_name <- str_detect(pred_name, " ")
L <- L[,c(correct_name)]

write_rds(L, "R/matrix_inter.RDS")






