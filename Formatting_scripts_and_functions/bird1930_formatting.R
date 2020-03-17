#set the working directory
setwd("/home/benjamin/Documents/credits_recherche/")

#libraries 
library(tidyverse)
library(stringr)
library(taxize)
source("R/formatting_scripts_and_functions/sep_pooled_many.R")
rm(rank_ref, theplantlist, apg_families, apg_orders)

#Original manip from the mangal project
folder.name <- 'Bird_1930'

#Read the files
filenames <- list.files(paste0("data/Cohen_ecoweb/",folder.name, "/raw"), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB23', 'WEB24', 'WEB25', 'WEB26')

#manually keeping only the columns with vertebrates
matrices.interaction[[1]] <- matrices.interaction[[1]][,-c(9,10,11)]
matrices.interaction[[2]] <- matrices.interaction[[2]][,-c(2,5,6,7)]
matrices.interaction[[3]] <- matrices.interaction[[3]][,-c(3,5,6,7,16,20,21,22)]
matrices.interaction[[4]] <- matrices.interaction[[4]][,-c(3,5,6,7,11,17,22,26)]

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~gather(.x, key = "sp_taxon_2", value = "value", -1)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value")))

#Funciton to depooled the grouped name
listafterdepooled <- sep_pooled_many(matrices.interaction, sep = "(\\s-\\s)|(\\, )")
matrices.interaction <- unlist(listafterdepooled[1], recursive = F)
names(matrices.interaction) <- c('WEB23', 'WEB24', 'WEB25', 'WEB26')
matrices.interaction <- map(matrices.interaction, ~.x[!(.x$sp_taxon_1=="etc." | .x$sp_taxon_2=='etc.'),])

#manually rearranging species pooled together
matrices.interaction[[3]] <- rbind(matrices.interaction[[3]], matrices.interaction[[3]][c(229:266),])
matrices.interaction[[3]][c(229:266),2] <- "hairy woodpecker"
matrices.interaction[[3]][c(989:1026),2] <- "downy woodpecker"
matrices.interaction[[3]] <- rbind(matrices.interaction[[3]], matrices.interaction[[3]][c(381:418),])
matrices.interaction[[3]][c(381:418),2] <- "Cooper's hawk"
matrices.interaction[[3]][c(1027:1064),2] <- "Sharpshinned hawk"

matrices.interaction[[4]] <- rbind(matrices.interaction[[4]], matrices.interaction[[4]][c(241:288),])
matrices.interaction[[4]][c(241:288),2] <- "hairy woodpecker"
matrices.interaction[[4]][c(1345:1392),2] <- "downy woodpecker"
matrices.interaction[[4]] <- rbind(matrices.interaction[[4]], matrices.interaction[[4]][c(289:336),])
matrices.interaction[[4]][c(289:336),2] <- "Cooper's hawk"
matrices.interaction[[4]][c(1393:1440),2] <- "Sharpshinned hawk"

#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value")))

#--------------------
#Taxa
#--------------------
name.dictionary <- read.csv2(file='data/Cohen_ecoweb/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB23.csv'), subset(name.dictionary, name.dictionary$web == 'WEB24.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB25.csv'), subset(name.dictionary, name.dictionary$web == 'WEB26.csv'))
names(names.of.web) <- c('WEB23', 'WEB24', 'WEB25', 'WEB26')

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, split_name, original_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$split_name), ])
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$scientific_name),])
df_with_manualones <- data.frame(c("hairy woodpecker","downy woodpecker","Cooper's hawk","Sharpshinned hawk"), c("hairy woodpecker","downy woodpecker","Cooper's hawk","Sharpshinned hawk"),c("Picoides villosus", "Picoides pubescens","Accipiter cooperii","Accipiter striatus"))
colnames(df_with_manualones) <- c("split_name","original_name","scientific_name")
names.of.web <- map(names.of.web, ~rbind(.x, df_with_manualones))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))

#Modifying manualy (pcq j'ai la flemme) the names.of.web to fit the names in matrices.interaction
names.of.web[[1]][4,1] <- "Richardson spermophile (ground squirrel)"
names.of.web[[1]][6,1] <- "vole (Microtus)"
names.of.web[[1]][7,1] <- "13-striped spermophile (ground squirrel)"
names.of.web[[1]][8,1] <- "pocket gopher (Thomomys)"
names.of.web[[1]][9,1] <- "insects in herb and surface stratum"
names.of.web[[1]][9,3] <- "insecta"
names.of.web[[1]][14,3] <- "insecta"
names.of.web[[1]] <- names.of.web[[1]][-c(5),]

names.of.web[[2]][6,3] <- "Disonycha quinquevittatum"
names.of.web[[2]][13,3] <- "Quiscalus quiscula"
names.of.web[[2]][15,1] <- "Maryland yellowthroat"
names.of.web[[2]][15,3] <- "Geothlypis trichas"

names.of.web[[3]][1,1] <- "spiders (mature forest)"
names.of.web[[3]][2,1]  <- "insects (mature forest)"
names.of.web[[3]][30,1] <- "redbacked vole (Evolomys)"
names.of.web[[3]] <- names.of.web[[3]][-37,]

names.of.web[[4]][17,1] <- "Maryland yellowthroat"
names.of.web[[4]][17,3] <- "Geothlypis trichas"
names.of.web[[4]][21,3] <-"Quiscalus quiscula"
names.of.web[[4]] <- names.of.web[[4]][-c(49,50),]


names.of.web <- map(names.of.web, ~select(.x, c(1,3)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(split_name == "canker", "canker", scientific_name)))
matrices.interaction[[1]]$sp_taxon_1 <- tolower(matrices.interaction[[1]]$sp_taxon_1)
matrices.interaction[[2]]$sp_taxon_1 <- tolower(matrices.interaction[[2]]$sp_taxon_1)
matrices.interaction[[3]]$sp_taxon_1 <- tolower(matrices.interaction[[3]]$sp_taxon_1)
matrices.interaction[[4]]$sp_taxon_1 <- tolower(matrices.interaction[[4]]$sp_taxon_1)
matrices.interaction[[1]]$sp_taxon_2 <- tolower(matrices.interaction[[1]]$sp_taxon_2)
matrices.interaction[[2]]$sp_taxon_2 <- tolower(matrices.interaction[[2]]$sp_taxon_2)
matrices.interaction[[3]]$sp_taxon_2 <- tolower(matrices.interaction[[3]]$sp_taxon_2)
matrices.interaction[[4]]$sp_taxon_2 <- tolower(matrices.interaction[[4]]$sp_taxon_2)
names.of.web[[1]]$split_name <- tolower(names.of.web[[1]]$split_name)
names.of.web[[2]]$split_name <- tolower(names.of.web[[2]]$split_name)
names.of.web[[3]]$split_name <- tolower(names.of.web[[3]]$split_name)
names.of.web[[4]]$split_name <- tolower(names.of.web[[4]]$split_name)
names.of.web[[1]]$scientific_name <- tolower(names.of.web[[1]]$scientific_name)
names.of.web[[2]]$scientific_name <- tolower(names.of.web[[2]]$scientific_name)
names.of.web[[3]]$scientific_name <- tolower(names.of.web[[3]]$scientific_name)
names.of.web[[4]]$scientific_name <- tolower(names.of.web[[4]]$scientific_name)

#####Filling scientific names in and the last columns
matrices.interaction <- map(matrices.interaction, ~cbind(.x, "pred_scientific", "prey_scientific","type_web","location","source"))
matrices.interaction <- map(matrices.interaction, ~(.x[,c(1,4,2,5,3,6,7,8)]))
matrices.interaction <- map(matrices.interaction, ~(`colnames<-`(.x, c("pred_common","pred_scientific","prey_common","prey_scientific","interaction","type_web","location","source"))))

                            
#web23
web23 <- matrices.interaction$WEB23
web23$pred_scientific <- names.of.web[[1]]$scientific_name[match(web23$pred_common, names.of.web[[1]]$split_name)]
web23$prey_scientific <- names.of.web[[1]]$scientific_name[match(web23$prey_common, names.of.web[[1]]$split_name)]
web23$type_web <- "extract"
web23$location <- "Manitoba"
web23$source <- "R. D. Bird, Biotic communities of the Aspen Parkland of central Canada,
     Ecology, 11:356-442, from p. 383 (1930)."
#web24
web24 <- matrices.interaction$WEB24
web24$pred_scientific <- names.of.web[[2]]$scientific_name[match(web24$pred_common, names.of.web[[2]]$split_name)]
web24$prey_scientific <- names.of.web[[2]]$scientific_name[match(web24$prey_common, names.of.web[[2]]$split_name)]
web24$type_web <- "extract"
web24$location <- "Manitoba"
web24$source <- "R. D. Bird, Biotic communities of the Aspen Parkland of central Canada,
     Ecology, 11:356-442, from p. 393 (1930)."
#web25
web25 <- matrices.interaction$WEB25
web25$pred_scientific <- names.of.web[[3]]$scientific_name[match(web25$pred_common, names.of.web[[3]]$split_name)]
web25$prey_scientific <- names.of.web[[3]]$scientific_name[match(web25$prey_common, names.of.web[[3]]$split_name)]
web25$type_web <- "extract"
web25$location <- "Manitoba"
web25$source <- "R. D. Bird, Biotic communities of the Aspen Parkland of central Canada,
     Ecology, 11:356-442, from p. 406 (1930)."
#web26
web26 <- matrices.interaction$WEB26
web26$pred_scientific <- names.of.web[[4]]$scientific_name[match(web26$pred_common, names.of.web[[4]]$split_name)]
web26$prey_scientific <- names.of.web[[4]]$scientific_name[match(web26$prey_common, names.of.web[[4]]$split_name)]
web26$type_web <- "extract"
web26$location <- "Manitoba"
web26$source <- "R. D. Bird, Biotic communities of the Aspen Parkland of central Canada,
     Ecology, 11:356-442, from p. 410 (1930)."

#changing the -1 to 1, based on Cohen
web23$interaction[web23$interaction == -1] <- 1
web24$interaction[web24$interaction == -1] <- 1
web25$interaction[web25$interaction == -1] <- 1
web26$interaction[web26$interaction == -1] <- 1

#manually fixing empty scientific names
web23$pred_scientific[web23$pred_common == "badger"] <- "mustelidae"
web23 <- drop_na(web23)
web25$prey_scientific[web25$prey_common == "insects (forest edge)"] <- "insecta"
web25$prey_scientific[web25$prey_common == "redbacked vole"] <- "myodes"
web25$pred_scientific[web25$pred_common == "redbacked vole"] <- "myodes"
web26$pred_scientific[web26$pred_common == "goshawk"] <- "accipiter gentilis"

#putting the scientific names back to capital letters
web23$pred_scientific <- paste0(toupper(substr(web23$pred_scientific, 1, 1)), substr(web23$pred_scientific, 2, nchar(web23$pred_scientific)))
web23$prey_scientific <- paste0(toupper(substr(web23$prey_scientific, 1, 1)), substr(web23$prey_scientific, 2, nchar(web23$prey_scientific)))
web24$pred_scientific <- paste0(toupper(substr(web24$pred_scientific, 1, 1)), substr(web24$pred_scientific, 2, nchar(web24$pred_scientific)))
web24$prey_scientific <- paste0(toupper(substr(web24$prey_scientific, 1, 1)), substr(web24$prey_scientific, 2, nchar(web24$prey_scientific)))
web25$pred_scientific <- paste0(toupper(substr(web25$pred_scientific, 1, 1)), substr(web25$pred_scientific, 2, nchar(web25$pred_scientific)))
web25$prey_scientific <- paste0(toupper(substr(web25$prey_scientific, 1, 1)), substr(web25$prey_scientific, 2, nchar(web25$prey_scientific)))
web26$pred_scientific <- paste0(toupper(substr(web26$pred_scientific, 1, 1)), substr(web26$pred_scientific, 2, nchar(web26$pred_scientific)))
web26$prey_scientific <- paste0(toupper(substr(web26$prey_scientific, 1, 1)), substr(web26$prey_scientific, 2, nchar(web26$prey_scientific)))

#write to csv
write.csv(web23, "data/Cohen_ecoweb/web23.csv")
write.csv(web24, "data/Cohen_ecoweb/web24.csv")
write.csv(web25, "data/Cohen_ecoweb/web25.csv")
write.csv(web26, "data/Cohen_ecoweb/web26.csv")


#taxonomy correction
web23 <- read.csv("data/Cohen_ecoweb/web23.csv", header = T)
web24 <- read.csv("data/Cohen_ecoweb/web24.csv", header = T)
web25 <- read.csv("data/Cohen_ecoweb/web25.csv", header = T)
web26 <- read.csv("data/Cohen_ecoweb/web26.csv", header = T)

web23 <- web23[!duplicated(web23[,c('pred_scientific','prey_scientific')]),]
web24 <- web24[!duplicated(web24[,c('pred_scientific','prey_scientific')]),]
web25 <- web25[!duplicated(web25[,c('pred_scientific','prey_scientific')]),]
web26 <- web26[!duplicated(web26[,c('pred_scientific','prey_scientific')]),]

web23$pred_scientific <- str_remove_all(web23$pred_scientific, '\\s\\(.*\\)$')
web23$prey_scientific <- str_remove_all(web23$prey_scientific, '\\s\\(.*\\)$')
web24$pred_scientific <- str_remove_all(web24$pred_scientific, '\\s\\(.*\\)$')
web24$prey_scientific <- str_remove_all(web24$prey_scientific, '\\s\\(.*\\)$')
web25$pred_scientific <- str_remove_all(web25$pred_scientific, '\\s\\(.*\\)$')
web25$prey_scientific <- str_remove_all(web25$prey_scientific, '\\s\\(.*\\)$')
web26$pred_scientific <- str_remove_all(web26$pred_scientific, '\\s\\(.*\\)$')
web26$prey_scientific <- str_remove_all(web26$prey_scientific, '\\s\\(.*\\)$')

pred_scientific_taxo_23 <- unique(as.character(web23$pred_scientific))
prey_scientific_taxo_23 <- unique(as.character(web23$prey_scientific))
pred_scientific_taxo_24 <- unique(as.character(web24$pred_scientific))
prey_scientific_taxo_24 <- unique(as.character(web24$prey_scientific))
pred_scientific_taxo_25 <- unique(as.character(web25$pred_scientific))
prey_scientific_taxo_25 <- unique(as.character(web25$prey_scientific))
pred_scientific_taxo_26 <- unique(as.character(web26$pred_scientific))
prey_scientific_taxo_26 <- unique(as.character(web26$prey_scientific))

pred_scientific_taxo_resolved_23 <- gnr_resolve(pred_scientific_taxo_23, best_match_only = T, canonical = T)
prey_scientific_taxo_resolved_23 <- gnr_resolve(prey_scientific_taxo_23, best_match_only = T, canonical = T)
pred_scientific_taxo_resolved_24 <- gnr_resolve(pred_scientific_taxo_24, best_match_only = T, canonical = T)
prey_scientific_taxo_resolved_24 <- gnr_resolve(prey_scientific_taxo_24, best_match_only = T, canonical = T)
pred_scientific_taxo_resolved_25 <- gnr_resolve(pred_scientific_taxo_25, best_match_only = T, canonical = T)
prey_scientific_taxo_resolved_25 <- gnr_resolve(prey_scientific_taxo_25, best_match_only = T, canonical = T)
pred_scientific_taxo_resolved_26 <- gnr_resolve(pred_scientific_taxo_26, best_match_only = T, canonical = T)
prey_scientific_taxo_resolved_26 <- gnr_resolve(prey_scientific_taxo_26, best_match_only = T, canonical = T)

web23$pred_scientific <- pred_scientific_taxo_resolved_23$matched_name2[match(web23$pred_scientific, pred_scientific_taxo_resolved_23$submitted_name)]
web23$prey_scientific <- prey_scientific_taxo_resolved_23$matched_name2[match(web23$prey_scientific, prey_scientific_taxo_resolved_23$submitted_name)]
web24$pred_scientific <- pred_scientific_taxo_resolved_24$matched_name2[match(web24$pred_scientific, pred_scientific_taxo_resolved_24$submitted_name)]
web24$prey_scientific <- prey_scientific_taxo_resolved_24$matched_name2[match(web24$prey_scientific, prey_scientific_taxo_resolved_24$submitted_name)]
web25$pred_scientific <- pred_scientific_taxo_resolved_25$matched_name2[match(web25$pred_scientific, pred_scientific_taxo_resolved_25$submitted_name)]
web25$prey_scientific <- prey_scientific_taxo_resolved_25$matched_name2[match(web25$prey_scientific, prey_scientific_taxo_resolved_25$submitted_name)]
web26$pred_scientific <- pred_scientific_taxo_resolved_26$matched_name2[match(web26$pred_scientific, pred_scientific_taxo_resolved_26$submitted_name)]
web26$prey_scientific <- prey_scientific_taxo_resolved_26$matched_name2[match(web26$prey_scientific, prey_scientific_taxo_resolved_26$submitted_name)]

web23 <- web23[,-1]
web24 <- web24[,-1]
web25 <- web25[,-1]
web26 <- web26[,-1]
#write it back to .csv
write.csv(web23, "data/Cohen_ecoweb/web23.csv", row.names = F)
write.csv(web24, "data/Cohen_ecoweb/web24.csv", row.names = F)
write.csv(web25, "data/Cohen_ecoweb/web25.csv", row.names = F)
write.csv(web26, "data/Cohen_ecoweb/web26.csv", row.names = F)

rm(list=ls())
