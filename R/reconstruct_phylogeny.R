library(taxize)

#Get the taxonomic hierarchy for each species for a dsitance measure in the KNN SCRIPT
L <- readRDS("data/intermediate_object_results/matrix_inter_wide.RDS")
pred_scientific_taxo <- rownames(L)
taxo_rank <- classification(pred_scientific_taxo, db = "gbif", rows = 1)

#Get the species for which we don't have the taxonomic hierarchy
vect_name <- c()
for(i in 1:length(taxo_rank)){
  if(nrow(as.data.frame(taxo_rank[i]))<2){vect_name <- append(vect_name, names(taxo_rank[i]))}
} #Looks like there isn't any species for which we weren't able to rebuild their taxonomic hierarchy

write_rds(taxo_rank, "data/intermediate_object_results/pred_phylogeny.RDS")

#for the species of quebec that were not in the inital fw. Will join them in next scripts
pred_scientific_taxo_miss <- readRDS("data/intermediate_object_results/sp_outof_fw.RDS")
taxo_rank_missingQc <- classification(pred_scientific_taxo_miss, db = "gbif")

write_rds(taxo_rank_missingQc, "data/intermediate_object_results/pred_phylogeny_miss.RDS")

rm(list=ls())




