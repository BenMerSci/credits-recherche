#libraries
library(stringr)
library(dplyr)
library(taxize)
#set the working directory
setwd("/home/benjamin/Documents/credits_recherche/data/Ropars_nordic_2018/ds_000582043/")

#Load the data
interaction_matrix <- read.table("Tundra_Nunavik_Trophic_relationships.txt", header=T, sep="\t", row.names=1)
species_list <- read.csv("Tundra_Nunavik_Trophic_relations_species_list-1.csv", sep=",", header=T)

#Manually check which columns are terrestrial vertebrates
#After checking with the meta data file(species_list), it seems that there are some species that are in the metadata, that are not in the matrix, example Gulo gulo. Therefor, all vertebrate species present in the matrix will manually be kept (even if they are marine), because I can't filter with the metadata (knowing which ones are terrestrial and which ones are marine), because the species don't fit...
vertebrates_col <- colnames(interaction_matrix)
vertebrates_col <- gsub("\\.", " ", vertebrates_col)
vertebrates_col <- gsub("[[:space:]][[:space:]]", " ", vertebrates_col)
vertebrates_row <- rownames(interaction_matrix)
verterates_row <- gsub("\\.", " ", vertebrates_row)
vertebrates_row <- gsub("[[:space:]]", " ", vertebrates_row)

interaction_matrix <- `colnames<-`(interaction_matrix, vertebrates_col) 
interaction_matrix <- `rownames<-`(interaction_matrix, vertebrates_row)

#Wanted to match the species in the metadata, but they don't fit..

# only_terrestrial <- grep("terrestrial", species_list$functional_group)
# only_aquatic <- grep("aquatic", species_list$functional_group)
# terrestrial_vert <- sort(append(only_terrestrial, only_aquatic))
# terrestrial_vert <- as.character(species_list[terrestrial_vert, "sp_latin"])

#matrix with only terrestrial vertebrates in columns

interaction_df <- as.data.frame(interaction_matrix[,c(14:213)]) %>%
  rownames_to_column(.) %>%
  pivot_longer(cols = 2:201, names_to = "pred_scientific") %>%
  `colnames<-`(c("prey_scientific","pred_scientific","interaction")) %>%
  .[order(as.data.frame(.[,'pred_scientific'])),]

#Adding the missing columns that complete the sets to match the full dataset
interaction_df$pred_common <- rep(NA, nrow(interaction_df))
interaction_df$prey_common <- rep(NA, nrow(interaction_df))
interaction_df$type_web <- rep("full", nrow(interaction_df))
interaction_df$location <- rep("Nunavik", nrow(interaction_df))
interaction_df$source <- rep("Berteaux, D., Ropars, P., Casajus, N. 2018. Toundra Nunavik : Matrice des relations trophiques entre espèces du Labrador et du Québec nordique, v. 1.0 (1980-2010). Nordicana D36, doi: 10.5885/45555CE-DA1FF11FA4254703.", nrow(interaction_df))
interaction_df <- interaction_df[,c(4,2,5,1,3,6,7,8)]

#export as .csv.
write.csv(interaction_df, "interactions_nunavik.csv", row.names=F)

rm(list=ls())

interaction_df <- read.csv("interactions_nunavik.csv", header=T, stringsAsFactors = FALSE)
interaction_df <- interaction_df[!duplicated(interaction_df[,c('pred_scientific','prey_scientific')]),]
interaction_df$prey_scientific[interaction_df$prey_scientific == "Branta canadensis "] <- "Branta canadensis"

#taxonomy correction
interaction_df$pred_scientific <- str_remove_all(interaction_df$pred_scientific, '\\s\\(.*\\)$')
interaction_df$prey_scientific <- str_remove_all(interaction_df$prey_scientific, '\\s\\(.*\\)$')

pred_scientific_taxo <- unique(as.character(interaction_df$pred_scientific))
prey_scientific_taxo <- unique(as.character(interaction_df$prey_scientific))

pred_scientific_taxo_resolved <- gnr_resolve(pred_scientific_taxo, best_match_only = T, canonical = T)
which(pred_scientific_taxo_resolved$matched_name2!=pred_scientific_taxo_resolved$submitted_name)

prey_scientific_taxo_resolved <- gnr_resolve(prey_scientific_taxo, best_match_only = T, canonical = T)
which(prey_scientific_taxo_resolved$matched_name2!=prey_scientific_taxo_resolved$submitted_name)
prey_taxo_subset_resolved <- prey_scientific_taxo_resolved[c(which(prey_scientific_taxo_resolved$matched_name2!=prey_scientific_taxo_resolved$submitted_name)),]
prey_scientific_taxo_resolved[c(207:208),5] <- c("Marine subsides", "Terrestrial subsides")
dropped_mat_prey <- as.data.frame(matrix(nrow = length(attr(prey_scientific_taxo_resolved, "not_known")), ncol = 5))
dropped_mat_prey[,1:2] <- attr(prey_scientific_taxo_resolved, "not_known")
colnames(dropped_mat_prey) <- colnames(prey_scientific_taxo_resolved)
prey_scientific_taxo_resolved <- rbind(prey_scientific_taxo_resolved, dropped_mat_prey)
prey_scientific_taxo_resolved[209,5] <- "Bryophyta"
prey_scientific_taxo_resolved[210,5] <- "Sedge"
prey_scientific_taxo_resolved[211,5] <- "Poaceae"
prey_scientific_taxo_resolved[212,5] <- "Forbs"
prey_scientific_taxo_resolved[213,5] <- "Low shrubs"
prey_scientific_taxo_resolved[214,5] <- "Erect shrubs"
prey_scientific_taxo_resolved[215,5] <- "Trees"
prey_scientific_taxo_resolved[216,5] <- "Aquatic subsides"



interaction_df$pred_scientific <- pred_scientific_taxo_resolved$matched_name2[match(interaction_df$pred_scientific, pred_scientific_taxo_resolved$submitted_name)]
interaction_df$prey_scientific <- prey_scientific_taxo_resolved$matched_name2[match(interaction_df$prey_scientific, prey_scientific_taxo_resolved$submitted_name)]

#Checking if any species is NA
sum(is.na(interaction_df$pred_scientific))
sum(is.na(interaction_df$prey_scientific))

#save it to .csv
write.csv(interaction_df, "interactions_nunavik.csv", row.names=F)
rm(list=ls())
