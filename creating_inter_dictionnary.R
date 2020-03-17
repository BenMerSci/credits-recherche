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

#removing duplicate interaction
meta_network$pred_scientific <- tolower(meta_network$pred_scientific)
meta_network$prey_scientific <- tolower(meta_network$prey_scientific)
meta_network <- meta_network[!duplicated(meta_network[,c('pred_scientific','prey_scientific')]),]
meta_network$pred_scientific <-  paste0(str_to_upper(str_extract(meta_network$pred_scientific, ".{1}")), str_remove(meta_network$pred_scientific, ".{1}"))
meta_network$prey_scientific <-  paste0(str_to_upper(str_extract(meta_network$prey_scientific, ".{1}")), str_remove(meta_network$prey_scientific, ".{1}"))


#write it to .csv
write.csv(meta_network, "data/intermediate_object_results/matrix_inter_long.csv", row.names = F)
rm(list=ls())
meta_network <- read.csv("data/intermediate_object_results/matrix_inter_long.csv", header = T)


#Final correction for taxo too, to see if we have duplicate but written different

#Removing the preds that are just genus and no species
meta_network <- meta_network[,c("pred_scientific","prey_scientific","interaction")]
meta_network$pred_scientific <- as.character(meta_network$pred_scientific)
meta_network$prey_scientific <- as.character(meta_network$prey_scientific)
meta_network$pred_scientific <- gsub("Falco peregrinus", "Falco peregrinus tundrius", meta_network$pred_scientific)  # checked in the dataset, and all the Falco pregrinus with no subspecies were from Bylot or Nunvavik, so I think it's reasonable to change them to the tundrius subsp.
meta_network$prey_scientific <- gsub("Falco peregrinus", "Falco peregrinus tundrius", meta_network$prey_scientific)
pred_name <- meta_network$pred_scientific
correct_name <- str_detect(pred_name, " ")
meta_network <- meta_network[c(correct_name),]

#Load the list of species from all Quebec to check which species in our metaweb is missing
qc_list <- read.csv("data/intermediate_object_results/liste_sp_qc.csv", header = T)
pred_name_metaweb <- unique(as.character(meta_network$pred_scientific))
pred_name_qc <- unique(as.character(qc_list[,"scientific_name"]))
diff_name <- pred_name_metaweb[!(pred_name_metaweb %in% pred_name_qc)]

#manually fixing the unvalid name that were found in diff_name, but not all names in diff_name are unvalid, some are just not quebec species therefor it's normal that we don't find them in the qc_list
meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Dendroica petechia", "Setophaga petechia")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Dendroica petechia", "Setophaga petechia")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Rana pipiens", "Lithobates pipiens")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Rana pipiens", "Lithobates pipiens")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Carduelis tristis", "Spinus tristis")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Carduelis tristis", "Spinus tristis")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Carduelis tristis", "Spinus tristis")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Carduelis tristis", "Spinus tristis")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Lemnus trimucronatus", "Lemmus trimucronatus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Lemnus trimucronatus", "Lemmus trimucronatus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Anser caerulescens atlantica", "Chen caerulescens")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Anser caerulescens atlantica", "Chen caerulescens")
meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Anser caerulescens", "Chen caerulescens")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Anser caerulescens", "Chen caerulescens")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Alopex lagopus", "Vulpes lagopus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Alopex lagopus", "Vulpes lagopus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Hyla versicolor", "Dryophytes versicolor")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Hyla versicolor", "Dryophytes versicolor")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Alces alces", "Alces americanus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Alces alces", "Alces americanus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Circus hudsonius", "Circus cyaneus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Circus hudsonius", "Circus cyaneus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Dicrostonyx groenlandicus", "Dicrostonyx hudsonius")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Dicrostonyx groenlandicus", "Dicrostonyx hudsonius")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Dryobates pubescens", "Picoides pubescens")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Dryobates pubescens", "Picoides pubescens")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Leuconotopicus villosus", "Picoides villosus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Leuconotopicus villosus", "Picoides villosus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Mareca americana", "Anas americana")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Mareca americana", "Anas americana")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Melanitta americana", "Melanitta nigra americana")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Melanitta americana", "Melanitta nigra americana")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Parkesia noveboracensis", "Seiurus noveboracensis")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Parkesia noveboracensis", "Seiurus noveboracensis")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Parus atricapillus", "Poecile atricapillus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Parus atricapillus", "Poecile atricapillus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Parus hudsonicus", "Poecile hudsonicus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Parus hudsonicus", "Poecile hudsonicus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Zapus hudsonicus", "Zapus hudsonius")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Zapus hudsonicus", "Zapus hudsonius")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Alnus crispa", "Alnus viridis crispa")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Alnus crispa", "Alnus viridis crispa")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Alnus rugosa", "Alnus incana rugosa")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Alnus rugosa", "Alnus incana rugosa")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Aster lanceolatus", "Symphyotrichum lanceolatum")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Aster lanceolatus", "Symphyotrichum lanceolatum")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Aster lateriflorus", "Symphyotrichum lateriflorum")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Aster lateriflorus", "Symphyotrichum lateriflorum")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Bryum tortifolium", "Bryum cyclophyllum")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Bryum tortifolium", "Bryum cyclophyllum")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Calypogeia trichomanis", "Calypogeia")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Calypogeia trichomanis", "Calypogeia")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Cerastium vulgatum", "Cerastium fontanum vulgare")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Cerastium vulgatum", "Cerastium fontanum vulgare")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Chenopodium hybridum", "Chenopodium simplex")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Chenopodium hybridum", "Chenopodium simplex")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Chiloscyphus rivularis", "Chiloscyphus polyanthos rivularis")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Chiloscyphus rivularis", "Chiloscyphus polyanthos rivularis")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Dendroica pinus", "Setophaga pinus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Dendroica pinus", "Setophaga pinus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Deschampsia flexuosa", "Avenella flexuosa")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Deschampsia flexuosa", "Avenella flexuosa")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Dicranum rugosum", "Dicranum polysetum")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Dicranum rugosum", "Dicranum polysetum")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Dryopteris spinulosa", "Dryopteris carthusiana")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Dryopteris spinulosa", "Dryopteris carthusiana")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Echinochloa crusgalli", "Echinochloa crus-galli crus-galli")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Echinochloa crusgalli", "Echinochloa crus-galli crus-galli")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Elytrigia repens", "Elymus repens ")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Elytrigia repens", "Elymus repens ")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Fissidens cristatus", "Fissidens dubius")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Fissidens cristatus", "Fissidens dubius")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Hypericum virginicum", "Triadenum virginicum")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Hypericum virginicum", "Triadenum virginicum")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Jungermannia cordifolia", "Jungermannia exsertifolia cordifolia")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Jungermannia cordifolia", "Jungermannia exsertifolia cordifolia")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Jungermannia lanceolata", "Jungermannia atrovirens")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Jungermannia lanceolata", "Jungermannia atrovirens")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Ledum groenlandicum", "Rhododendron groenlandicum")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Ledum groenlandicum", "Rhododendron groenlandicum")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Ligusticum scothicum", "Ligusticum scoticum hultenii")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Ligusticum scothicum", "Ligusticum scoticum hultenii")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Nuphar variegatum", "Nuphar variegata")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Nuphar variegatum", "Nuphar variegata")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Osteichthyes", "Gnathostomata")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Osteichthyes", "Gnathostomata")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Plantago juncoides", "Plantago maritima juncoides")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Plantago juncoides", "Plantago maritima juncoides")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Pohlia gracilis", "Pohlia filum")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Pohlia gracilis", "Pohlia filum")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Polygonum arifolium", "Persicaria arifolia")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Polygonum arifolium", "Persicaria arifolia")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Polygonum hydropiper", "Persicaria hydropiper")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Polygonum hydropiper", "Persicaria hydropiper")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Polygonum lapathifolium", "Persicaria lapathifolia")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Polygonum lapathifolium", "Persicaria lapathifolia")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Polygonum pensylvanicum", "Persicaria pensylvanica")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Polygonum pensylvanicum", "Persicaria pensylvanica")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Polygonum viviparum", "Bistorta vivipara")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Polygonum viviparum", "Bistorta vivipara")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Potentilla arguta", "Drymocallis arguta")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Potentilla arguta", "Drymocallis arguta")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Potentilla fruticosa", "Dasiphora fruticosa")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Potentilla fruticosa", "Dasiphora fruticosa")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Potentilla palustris", "Comarum palustre")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Potentilla palustris", "Comarum palustre")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Pyrus americana", "Sorbus americana")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Pyrus americana", "Sorbus americana")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Pyrus floribunda", "Pyrus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Pyrus floribunda", "Pyrus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Salix longifolia", "Salix interior")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Salix longifolia", "Salix interior")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Scirpus cespitosus", "Trichophorum cespitosum")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Scirpus cespitosus", "Trichophorum cespitosum")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Scirpus validus", "Schoenoplectus tabernaemontani")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Scirpus validus", "Schoenoplectus tabernaemontani")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Setaria glauca", "Cenchrus americanus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Setaria glauca", "Cenchrus americanus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Spirea tomentosa", "Spiraea tomentosa")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Spirea tomentosa", "Spiraea tomentosa")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Streptopus roseus", "Streptopus lanceolatus roseus")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Streptopus roseus", "Streptopus lanceolatus roseus")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Thalictrum polygamum", "Thalictrum pubescens")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Thalictrum polygamum", "Thalictrum pubescens")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Thelypteris hexagonoptera", "Phegopteris hexagonoptera")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Thelypteris hexagonoptera", "Phegopteris hexagonoptera")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Thelypteris phegopteris", "Phegopteris connectilis")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Thelypteris phegopteris", "Phegopteris connectilis")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Trifolium agrarium", "Trifolium aureum")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Trifolium agrarium", "Trifolium aureum")

meta_network$pred_scientific <- str_replace(meta_network$pred_scientific, "Viburnum cassinoides", "Viburnum nudum cassinoides")
meta_network$prey_scientific <- str_replace(meta_network$prey_scientific, "Viburnum cassinoides", "Viburnum nudum cassinoides")

# vector of names that popped out when I used: taxize::classification(species_of_metawweb, db = "gbif") so noted them by hand, since taxize::gnr_resolve didnt match any
#unvalid_sp <- c("Algea","Alnus crispa","Alnus rugosa","Amelanchier sanguinea","Aster lanceolatus","Aster lateriflorus","Bryum tortifolium","Calypogeia trichomanis","Cerastium vulgatum","Chenopodium hybridum","Chiloscyphus rivularis","Dendroica pinus","Deschampsia flexuosa","Dicranum rugosum","Dryopteris spinulosa","Echinochloa crusgalli", "Elymus trachycaulus", "Elytrigia repens", "Falco peregrinus tundrius anatum", "Fissidens cristatus", "Hypericum virginicum", "Jungermannia cordifolia", "Jungermannia lanceolata", "Ledum groenlandicum", "Ligusticum scothicum", "Mnium affine", "Nuphar variegatum", "Osteichthyes", "Phasianinae", "Plantago juncoides", "Pohlia gracilis", "Polygonum arifolium", "Polygonum hydropiper", "Polygonum lapathifolium", "Polygonum pensylvanicum", "Polygonum viviparum", "Potentilla anserina", "Potentilla arguta", "Potentilla fruticosa", "Potentilla palustris", "Pottia truncata", "Prunus virginiana", "Pyrus americana", "Pyrus floribunda", "Rhamnus alnifolia", "Salix longifolia", "Scirpus cespitosus", "Scirpus validus", "Setaria glauca", "Spirea tomentosa", "Streptopus roseus", "Thalictrum polygamum", "Thelypteris hexagonoptera", "Thelypteris phegopteris", "Trees", "Trifolium agrarium", "Viburnum cassinoides")

#duplicate_interaction 
# duplicated_inter <- duplicated(meta_network)
# meta_network <- meta_network[!duplicated_inter,]
meta_network <- unique(meta_network)
#duplicated(meta_network, by = c("pred_scientific","prey_scientific")) | duplicated(meta_network, by = c("pred_scientific","prey_scientific", fromLast = TRUE))

write_rds(meta_network, "data/intermediate_object_results/matrix_inter_long.RDS")
rm(list=ls())
meta_network <- readRDS("data/intermediate_object_results/matrix_inter_long.RDS")


#Making the matrix a square matrix for the  base network (species that were already in the system)
sp_names <- unique(c(meta_network$pred_scientific, meta_network$prey_scientific))
L <- expand.grid(sp_names, sp_names, stringsAsFactors = FALSE)
L <- L[,c(2,1)]
colnames(L) <- c("pred_scientific","prey_scientific")
L <- merge(meta_network, L, by=c("pred_scientific","prey_scientific"), all.y = TRUE)
L <- L[with(L, order(pred_scientific, interaction)),] #order the columns so for each species the interaction comes in that order : NA, 0, 1.
duplicated_inter <-  which(!(duplicated(L[c("pred_scientific","prey_scientific")], fromLast = TRUE)) == FALSE) #will get the first duplicate, so it's gonna be the inter = 0 because of last line
#L[!(duplicated(L[c("pred_scientific","prey_scientific")]))] | duplicated(L[c("pred_scientific","prey_scientific")], fromLast = TRUE)), ]
L <- L[-(duplicated_inter),]
L <- spread(L, prey_scientific, interaction)
rownames(L) <- L[,1]
L <- L[,-1]
L <- as.matrix(L)
write_rds(L, "data/intermediate_object_results/matrix_inter_wide.RDS")
rm(list=setdiff(ls(), "L"))

# Creating the whole species matrix (adding the species of quebec that were not in the system yet)
L_allQc <- L
qc_list <- read.csv("data/intermediate_object_results/liste_sp_qc.csv", header = TRUE)
old_sp <- as.character(rownames(L))
new_sp <- as.character(qc_list$scientific_name)
sub_new <- new_sp[!(new_sp%in% old_sp)]
write_rds(sub_new, "data/intermediate_object_results/sp_outof_fw.RDS")

sub_new_row <- matrix(NA, nrow = length(sub_new), ncol = ncol(L))
rownames(sub_new_row) <- sub_new
colnames(sub_new_row) <- colnames(L)
L_allQc <- rbind(L, sub_new_row)

sub_new_col <- matrix(NA, nrow = nrow(L_allQc), ncol = length(sub_new))
colnames(sub_new_col) <- sub_new

L_allQc <- cbind(L_allQc, sub_new_col)

write_rds(L_allQc, "data/intermediate_object_results/matrix_inter_wide_all.RDS")

rm(list=ls())
