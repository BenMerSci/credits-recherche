#set the working directory
setwd("/home/benjamin/Documents/credits_recherche/data/leroux_2014")

#libraries
library(readxl)
library(taxize)

#load the data
interaction_df <- read_excel("NL_FullDB_scinames.xlsx")

interaction_df$`Focal species` <- tolower(interaction_df$`Focal species`)
interaction_df$`Food item` <- tolower(interaction_df$`Food item`)

#unique the interactions
interaction_df <- interaction_df[!duplicated(interaction_df[,c("Focal species","Food item")]),]

#Adding the missing columns that complete the sets to match the full dataset
interaction_df$pred_common <- rep(NA, nrow(interaction_df))
interaction_df$prey_common <- rep(NA, nrow(interaction_df))
interaction_df$interaction <- 1
interaction_df$type_web <- rep("extract", nrow(interaction_df))
interaction_df <- interaction_df[,c(5,1,6,2,7,8,3,4)]
colnames(interaction_df) <- c("pred_common","pred_scientific","prey_common","prey_scientific","interaction","type_web","location","source")

#taxonomy correction
pred_scientific_taxo <- unique(as.character(interaction_df$pred_scientific))
prey_scientific_taxo <- unique(as.character(interaction_df$prey_scientific))

pred_scientific_taxo_resolved <- gnr_resolve(pred_scientific_taxo, best_match_only = T, canonical = T)
which(pred_scientific_taxo_resolved$matched_name2!=pred_scientific_taxo_resolved$submitted_name)
pred_scientific_taxo_resolved$submitted_name <- tolower(pred_scientific_taxo_resolved$submitted_name)

prey_scientific_taxo_resolved <- gnr_resolve(prey_scientific_taxo, best_match_only = T, canonical = T)
which(prey_scientific_taxo_resolved$matched_name2!=prey_scientific_taxo_resolved$submitted_name)
prey_taxo_subset_resolved <- prey_scientific_taxo_resolved[c(which(prey_scientific_taxo_resolved$matched_name2!=prey_scientific_taxo_resolved$submitted_name)),]
prey_scientific_taxo_resolved$matched_name2[prey_scientific_taxo_resolved$matched_name2 == "Human"] <- "Human food waste"
prey_scientific_taxo_resolved$matched_name2[prey_scientific_taxo_resolved$matched_name2 == "Felis"] <- "Felis catus"
prey_scientific_taxo_resolved$submitted_name <- tolower(prey_scientific_taxo_resolved$submitted_name)

interaction_df$pred_scientific <- pred_scientific_taxo_resolved$matched_name2[match(interaction_df$pred_scientific, pred_scientific_taxo_resolved$submitted_name)]
interaction_df$prey_scientific <- prey_scientific_taxo_resolved$matched_name2[match(interaction_df$prey_scientific, prey_scientific_taxo_resolved$submitted_name)]

#write back to csv to finish a clean by hand
write.csv(interaction_df, "leroux_2014.csv", row.names=F)
rm(list=ls())

