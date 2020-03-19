#libraries
library(taxize)
rm(apg_families,apg_orders,rank_ref,theplantlist)
#set the wd
setwd("/home/benjamin/Documents/credits_recherche/data/legagneux_2014")

#read the data
interaction_df <- read.csv("interactions_alert_bylot.csv", header=T, stringsAsFactors = F)
pred_scientific_taxo <- unique(as.character(interaction_df$pred_scientific))
prey_scientific_taxo <- unique(as.character(interaction_df$prey_scientific))

#taxize
pred_scientific_taxo_resolved <- gnr_resolve(pred_scientific_taxo, best_match_only = T, canonical = T)
prey_scientific_taxo_resolved <- gnr_resolve(prey_scientific_taxo, best_match_only = T, canonical = T)

#changing the taxonomy for the resolved one from gnr_resolve
interaction_df$prey_scientific[interaction_df$prey_scientific == "Aranea"] <- "Araneae" #this one changed manualy

interaction_df$pred_scientific <- pred_scientific_taxo_resolved$matched_name2[match(interaction_df$pred_scientific, pred_scientific_taxo_resolved$submitted_name)]
interaction_df$prey_scientific <- prey_scientific_taxo_resolved$matched_name2[match(interaction_df$prey_scientific, prey_scientific_taxo_resolved$submitted_name)]

#deleting duplicated interactions
interaction_df <- interaction_df[!duplicated(interaction_df[,c('pred_scientific','prey_scientific')]),]

#save it back to csv
write.csv(interaction_df, "interactions_alert_bylot.csv", row.names=F)
rm(list=ls())
