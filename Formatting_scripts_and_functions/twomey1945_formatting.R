setwd("/home/benjamin/Documents/credits_recherche/")

#libraries 
library(tidyverse)
library(reshape)
library(taxize)
source("R/sep_pooled.R")

#lod the data
name.dictionary <- read.csv2(file='data/Cohen_ecoweb/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
interaction_df <- read.csv('data/Cohen_ecoweb/Twomey_1945/raw/WEB59.csv', header= T, sep=',', stringsAsFactors = F)

#Getting only the web related names and filling the NA's scientific names with their original name
name.of.web <- subset(name.dictionary, name.dictionary$web == paste0('WEB59.csv'))
name.of.web <- select(name.of.web, split_name, original_name, scientific_name)
name.of.web[c(1:6,8),3] <- "Plantae"
name.of.web[2,1] <- "Sap"
name.of.web[7,3] <- "Detritus"
name.of.web[19,3] <- "Insecta"
name.of.web[28,1] <- "maryland yellow-throat"
#name.of.web <- name.of.web[!duplicated(name.of.web$original_name), ]
name.of.web$split_name <-  paste0(str_to_upper(str_extract(name.of.web$split_name, ".{1}")), str_remove(name.of.web$split_name, ".{1}"))
name.of.web$scientific_name <-  paste0(str_to_upper(str_extract(name.of.web$scientific_name, ".{1}")), str_remove(name.of.web$scientific_name, ".{1}"))

#Edge format
interaction_df <- interaction_df[,c(1,7,9,12:14,16:25)]
colnames(interaction_df) = interaction_df[1,]
interaction_df <- interaction_df[-1,]
interaction_df <- melt(interaction_df, id.vars = c(1), na.rm = TRUE)
names(interaction_df) <- c("sp_taxon_1", "sp_taxon_2", "value")

#Funciton to depooled the grouped name
listafterdepooled <- sep_pooled(interaction_df, sep = "(\\s-\\s|[:space:]and[:space:])")
interaction_df <- listafterdepooled[1]
interaction_df <- interaction_df$depooled
interaction_df$sp_taxon_1 <-  paste0(str_to_upper(str_extract(interaction_df$sp_taxon_1, ".{1}")), str_remove(interaction_df$sp_taxon_1, ".{1}"))
interaction_df$sp_taxon_2 <-  paste0(str_to_upper(str_extract(interaction_df$sp_taxon_2, ".{1}")), str_remove(interaction_df$sp_taxon_2, ".{1}"))

interaction_df <- cbind(interaction_df, "pred_scientific", "prey_scientific","type_web","location","source")
interaction_df <- interaction_df[,c(2,4,1,5,3,6,7,8)]
colnames(interaction_df) <- c("pred_common","pred_scientific","prey_common","prey_scientific","interaction","type_web","location","source")
interaction_df$type_web <- "extract"
interaction_df$location <- "Illinois"
interaction_df$source <- "A. C. Twomey, The bird population of an elm-maple forest with special reference
     to aspection, territorialism, and coactions, Ecol. Monogr. 15(2):175-205,
     from p. 202 (1945)."

interaction_df$pred_scientific <- name.of.web$scientific_name[match(interaction_df$pred_common, name.of.web$split_name)]
interaction_df$prey_scientific <- name.of.web$scientific_name[match(interaction_df$prey_common, name.of.web$split_name)]

pred_scientific_taxo <- unique(as.character(interaction_df$pred_scientific))
prey_scientific_taxo <- unique(as.character(interaction_df$prey_scientific))

pred_scientific_taxo_resolved <- gnr_resolve(pred_scientific_taxo, best_match_only = T, canonical = T)
which(pred_scientific_taxo_resolved$matched_name2!=pred_scientific_taxo_resolved$submitted_name)
pred_scientific_taxo_resolved[14,"matched_name2"] <- "Felis catus"
prey_scientific_taxo_resolved <- gnr_resolve(prey_scientific_taxo, best_match_only = T, canonical = T)
which(prey_scientific_taxo_resolved$matched_name2!=prey_scientific_taxo_resolved$submitted_name)

interaction_df$pred_scientific <- pred_scientific_taxo_resolved$matched_name2[match(interaction_df$pred_scientific, pred_scientific_taxo_resolved$submitted_name)]
interaction_df$prey_scientific <- prey_scientific_taxo_resolved$matched_name2[match(interaction_df$prey_scientific, prey_scientific_taxo_resolved$submitted_name)]

#write the file to .csv
write.csv(interaction_df, "data/Cohen_ecoweb/web59.csv", row.names = F)
rm(list=ls())