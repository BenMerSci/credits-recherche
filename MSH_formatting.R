#libraries needed
library(R.matlab)
library(tidyr)
library(tibble)
library(taxize)
rm(apg_families,apg_orders,rank_ref,theplantlist)

#set the WD
setwd("/home/benjamin/Documents/credits_recherche/data/MSH_2010/MSH_food_web_original/MSH_food web_original/1_Working files")

#Load the files
msh_pred_matrix <- readMat("MSH_data_nag_pred_matrix.mat")
data_category <- read.csv("foodweb-categories_manually_modified.csv", header=T, sep = ",")

#Formatting them
pred_matrix <- msh_pred_matrix$T.pred %>%
  `row.names<-`(., msh_pred_matrix$T.ID) %>%
  `colnames<-`(., msh_pred_matrix$T.ID) ## row and col names of matrix are now species's ID
  
#getting only the interactions for the vertebrates
vertebrates_ID <- data_category[data_category$Category.name=="AMPHIBIANS" | data_category$Category.name=="REPTILES" | data_category$Category.name=="BIRDS" | data_category$Category.name=="MAMMALS",] 


#subsetting pred_matrix columns in function of the vertebrates ID
temp <- which(colnames(pred_matrix) %in% vertebrates_ID$Species.number)
pred_matrix <- pred_matrix[,temp]

#put pred_matrix in long format
interaction_df <- as.data.frame(pred_matrix) %>%
rownames_to_column(.) %>%
pivot_longer(cols = 2:215, names_to = "pred_scientific") %>%
`colnames<-`(c("prey_scientific","pred_scientific","interaction")) %>%
.[order(as.data.frame(.[,'pred_scientific'])),]

interaction_df$prey_scientific <- data_category$Species.name[match(interaction_df$prey_scientific, data_category$Species.number)]
interaction_df$pred_scientific <- data_category$Species.name[match(interaction_df$pred_scientific, data_category$Species.number)]
interaction_df <- interaction_df[,c(2,1,3)]

#Adding the missing columns that complete the sets to match the full dataset
interaction_df$pred_common <- rep(NA, nrow(interaction_df))
interaction_df$prey_common <- rep(NA, nrow(interaction_df))
interaction_df$type_web <- rep("full", nrow(interaction_df))
interaction_df$location <- rep("Mont_StHilaire", nrow(interaction_df))
interaction_df$source <- rep("Parrott, L., 2010. Measuring ecological complexity. Ecological Indicators, 10(6), pp.1069–1076. Available at: http://dx.doi.org/10.1016/j.ecolind.2010.03.014.", nrow(interaction_df))
interaction_df <- interaction_df[,c(4,1,5,2,3,6,7,8)]

#export as .csv.
write.csv(interaction_df, "interactions_msh.csv", row.names=F)

rm(list=ls())

#Reloading the matrix to check the taxonomy
interaction_df <- read.csv("interactions_msh.csv", header=T)
interaction_df <- interaction_df[!duplicated(interaction_df[,c('pred_scientific','prey_scientific')]),]

pred_scientific_taxo <- unique(as.character(interaction_df$pred_scientific))
prey_scientific_taxo <- unique(as.character(interaction_df$prey_scientific))

pred_scientific_taxo_resolved <- gnr_resolve(pred_scientific_taxo, best_match_only = T, canonical = T)
which(pred_scientific_taxo_resolved$matched_name2!=pred_scientific_taxo_resolved$submitted_name)

prey_scientific_taxo_resolved <- gnr_resolve(prey_scientific_taxo, best_match_only = T, canonical = T)
which(prey_scientific_taxo_resolved$matched_name2!=prey_scientific_taxo_resolved$submitted_name)
prey_taxo_subset_resolved <- prey_scientific_taxo_resolved[c(which(prey_scientific_taxo_resolved$matched_name2!=prey_scientific_taxo_resolved$submitted_name)),]

interaction_df$pred_scientific <- pred_scientific_taxo_resolved$matched_name2[match(interaction_df$pred_scientific, pred_scientific_taxo_resolved$submitted_name)]
interaction_df$prey_scientific <- prey_scientific_taxo_resolved$matched_name2[match(interaction_df$prey_scientific, prey_scientific_taxo_resolved$submitted_name)]


#save it to .csv
write.csv(interaction_df, "interactions_msh.csv", row.names=F)
rm(list=ls())
