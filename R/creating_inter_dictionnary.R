#libraries
library(stringr)
library(taxize)
library(plyr)
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
write_rds(meta_network, "data/intermediate_object_results/matrix_inter_long.RDS")
rm(list=ls())
meta_network <- readRDS("data/intermediate_object_results/matrix_inter_long.RDS")


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

####################################################################################################################################################################
##################### VERY CHAOTIC SECTION WHERE I COMBINE DIFFERENT BADLY RESOLVED TAXONOMY TOGETHER, TO MAKE TE NETWORK LESS BIG #################################
####################################################################################################################################################################

# Section to get the names of the species that are prey, to combine all trees, plants, invertebrates into more succinctcategories
pred_name <- unique(meta_network$pred_scientific) # Get the predators' name
prey_name <- unique(meta_network$prey_scientific) # Get the preys' name
prey_name <- prey_name[-which(prey_name %in% pred_name)] # Get only the real preys
#phylo_prey <- classification(prey_name, db = "gbif", rows = 1) # Get their phylogeny if possible
#write_rds(phylo_prey, "data/intermediate_object_results/prey_classification.RDS")
phylo_prey <- readRDS("data/intermediate_object_results/prey_classification.RDS")
phylo_prey_list <- unclass(phylo_prey)

# Plant section
plantae <- phylo_prey_list[which(lapply(phylo_prey_list, function(x) which(x[[1]] == "Plantae")) == 1)]
trees <- c("Corylus","Prunus","Alnus viridis crispa","Populus tremuloides","Picea mariana","Betula michauxii","Betula pumila","Pinus resinosa","Ulmus americana","Juglans cinerea","Betula alleghaniensis","Ostrya virginiana","Populus deltoides","Salix nigra","Amelanchier arborea","Acer pensylvanicum","Fraxinus nigra","Salix interior","Amelanchier","Alnus incana rugosa","Prunus pennsylvanica","Amelanchier bartramiana","Sorbus","Betula","Carya","Picea","Alnus sinuata","Pinus strobus","Ulmus rubra","Fagus grandifolia","Betula populifolia","Populus grandidentata","Salix rigida","Amelanchier laevis","Prunus pensylvanica","Sorbus decora","Acer saccharum","Populus","Abies balsamea","Betula papyrifera","Sorbus americana","Larix laricina","Acer","Fagus","Taxus canadensis","Thuja occidentalis","Alnus incana", "Tsuga canadensis","Carya cordiformis","Quercus macrocarpa","Populus alba","Salix bebbiana","Amelanchier sanguinea","Prunus serotina","Acer saccharinum","Salix discolor","Cornus","Quercus","Acer rubrum","Picea glauca","Acer spicatum","Alnus","Betula glandulosa","Salix pellita","Quercus rubra","Populus balsamifera","Salix gracilis","Prunus virginiana","Acer nigrum","Fraxinus americana","Salix petiolaris","Salix","Salix alba","Carya ovata","Trees") # add "Trees" to trees
bryophyta <- plantae[which(lapply(plantae, function(x) which(x[2,] == "Bryophyta")) == 1)]
hepatics <- plantae[which(lapply(plantae, function(x) which(x[2,] == "Marchantiophyta")) == 1)]
lichens <- plantae[which(lapply(plantae, function(x) which(x[2,] == "Lichen")) == 1)]
plantae_to_rm <- c(trees, names(bryophyta), names(hepatics), names(lichens))

herbaceous <- which(names(plantae) %in% plantae_to_rm == F)
herbaceous <- plantae[herbaceous]
herbaceous <- c(names(herbaceous), "Forbs", "Sedge", "Low shrubs", "Erect shrubs")
herbaceous <- herbaceous[-483]
Bryophyta_hepatics_lichens <- c(names(bryophyta),names(hepatics),names(lichens), "Algae")

# Animal section    # Add Oligochaeta to invertebrates, remove "Salix"
animalia <- phylo_prey_list[which(lapply(phylo_prey_list, function(x) which(x[[1]] == "Animalia")) == 1)]
# Chordata
chordata <- animalia[which(lapply(animalia, function(x) which(x[2,] == "Chordata")) == 1)]

fish <- animalia[which(lapply(animalia, function(x) which(x[3,] == "Actinopterygii")) == 1)]
reptilia <- animalia[which(lapply(animalia, function(x) which(x[3,] == "Reptilia")) == 1)]
aves <- animalia[which(lapply(animalia, function(x) which(x[3,] == "Aves")) == 1)]
rodentia <- animalia[which(lapply(animalia, function(x) which(x[4,] == "Rodentia")) == 1)]

chordata_to_rm <- c(names(fish), names(reptilia), names(aves), names(rodentia))
chordata_to_rm <- which(names(chordata) %in% chordata_to_rm == F)
chordata_to_rm <- chordata[chordata_to_rm] #Leaving them like that

arthropoda <- animalia[which(lapply(animalia, function(x) which(x[2,] == "Arthropoda")) == 1)]

animalia_to_rm <- c(names(fish), names(reptilia), names(aves), names(rodentia), names(chordata_to_rm), names(arthropoda))
animalia_rest <- which(names(animalia) %in% animalia_to_rm == F)
animalia_rest <- animalia[animalia_rest]
invertebrates <- animalia_rest


fish <- c(names(fish), "Pisces")
reptilia <- c(names(reptilia), "Serpentes")
invertebrates <- c(names(invertebrates), "Oligochaeta", "Invertebrata", "Pelecypods")
invertebrates <- invertebrates[-c(14)]
arthropoda <- c(names(arthropoda), "Crustacea", "Eufernoia stygica")
arthropoda <- arthropoda[-c(31,50)]
chordata_to_rm <- names(chordata_to_rm)
chordata_to_rm <- chordata_to_rm[-c(6,35)]
aves <- names(aves)
aves <- aves[-c(8,16,17,18,22,23)]
rodentia <- c(names(rodentia), "Marmotini")
rodentia <- rodentia[-c(7,9,10,12,13,14)]

# Fungi section
fungi <- phylo_prey_list[which(lapply(phylo_prey_list, function(x) which(x[[1]] == "Fungi")) == 1)]
fungi <- names(fungi)
fungi <- fungi[-c(8)]

# Names NA section
names_na <- phylo_prey_list[which(is.na(phylo_prey_list))]
names_na <- names(names_na)
detritus <- names_na[c(2,4,5,13,14,15)]
detritus <- c(detritus, "Carrion")

rm(list=setdiff(ls(), c("trees","herbaceous","Bryophyta_hepatics_lichens","fish","reptilia","invertebrates","arthropoda","chordata_to_rm","aves","rodentia","fungi","detritus")))

######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################

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

# Grouping Arthropoda
L_arthropoda_col <- L[,arthropoda]
L_arthropoda_col_na <- which(rowSums(is.na(L_arthropoda_col)) == ncol(L_arthropoda_col))
arthropoda_col <- apply(L_arthropoda_col, 1, function(x) sum(x, na.rm = TRUE))
arthropoda_col[L_arthropoda_col_na] <- NA
L_arthropoda_row <- L[arthropoda,]
L_arthropoda_row_na <- which(colSums(is.na(L_arthropoda_row)) == nrow(L_arthropoda_row))
arthropoda_row <- apply(L_arthropoda_row, 2, function(x) sum(x, na.rm= TRUE))
arthropoda_row[L_arthropoda_row_na] <- NA
# L <- L[!row.names(L) %in% arthropoda,]
# L <- L[,!colnames(L) %in% arthropoda]
# Grouping aves
L_aves_col <- L[,aves]
L_aves_col_na <- which(rowSums(is.na(L_aves_col)) == ncol(L_aves_col))
aves_col <- apply(L_aves_col, 1, function(x) sum(x, na.rm = TRUE))
aves_col[L_aves_col_na] <- NA
L_aves_row <- L[aves,]
L_aves_row_na <- which(colSums(is.na(L_aves_row)) == nrow(L_aves_row))
aves_row <- apply(L_aves_row, 2, function(x) sum(x, na.rm= TRUE))
aves_row[L_aves_row_na] <- NA
# L <- L[!row.names(L) %in% aves,]
# L <- L[,!colnames(L) %in% aves]
# Grouping Bryophyta_hepatics_lichens
L_Bryophyta_hepatics_lichens_col <- L[,Bryophyta_hepatics_lichens]
L_Bryophyta_hepatics_lichens_col_na <- which(rowSums(is.na(L_Bryophyta_hepatics_lichens_col)) == ncol(L_Bryophyta_hepatics_lichens_col))
Bryophyta_hepatics_lichens_col <- apply(L_Bryophyta_hepatics_lichens_col, 1, function(x) sum(x, na.rm = TRUE))
Bryophyta_hepatics_lichens_col[L_Bryophyta_hepatics_lichens_col_na] <- NA
L_Bryophyta_hepatics_lichens_row <- L[Bryophyta_hepatics_lichens,]
L_Bryophyta_hepatics_lichens_row_na <- which(colSums(is.na(L_Bryophyta_hepatics_lichens_row)) == nrow(L_Bryophyta_hepatics_lichens_row))
Bryophyta_hepatics_lichens_row <- apply(L_Bryophyta_hepatics_lichens_row, 2, function(x) sum(x, na.rm= TRUE))
Bryophyta_hepatics_lichens_row[L_Bryophyta_hepatics_lichens_row_na] <- NA
# L <- L[!row.names(L) %in% Bryophyta_hepatics_lichens,]
# L <- L[,!colnames(L) %in% Bryophyta_hepatics_lichens]
# Grouping detritus
L_detritus_col <- L[,detritus]
L_detritus_col_na <- which(rowSums(is.na(L_detritus_col)) == ncol(L_detritus_col))
detritus_col <- apply(L_detritus_col, 1, function(x) sum(x, na.rm = TRUE))
detritus_col[L_detritus_col_na] <- NA
L_detritus_row <- L[detritus,]
L_detritus_row_na <- which(colSums(is.na(L_detritus_row)) == nrow(L_detritus_row))
detritus_row <- apply(L_detritus_row, 2, function(x) sum(x, na.rm= TRUE))
detritus_row[L_detritus_row_na] <- NA
# L <- L[!row.names(L) %in% detritus,]
# L <- L[,!colnames(L) %in% detritus]
# Grouping fish
L_fish_col <- L[,fish]
L_fish_col_na <- which(rowSums(is.na(L_fish_col)) == ncol(L_fish_col))
fish_col <- apply(L_fish_col, 1, function(x) sum(x, na.rm = TRUE))
fish_col[L_fish_col_na] <- NA
L_fish_row <- L[fish,]
L_fish_row_na <- which(colSums(is.na(L_fish_row)) == nrow(L_fish_row))
fish_row <- apply(L_fish_row, 2, function(x) sum(x, na.rm= TRUE))
fish_row[L_fish_row_na] <- NA
# L <- L[!row.names(L) %in% fish,]
# L <- L[,!colnames(L) %in% fish]
# Grouping fungi
L_fungi_col <- L[,fungi]
L_fungi_col_na <- which(rowSums(is.na(L_fungi_col)) == ncol(L_fungi_col))
fungi_col <- apply(L_fungi_col, 1, function(x) sum(x, na.rm = TRUE))
fungi_col[L_fungi_col_na] <- NA
L_fungi_row <- L[fungi,]
L_fungi_row_na <- which(colSums(is.na(L_fungi_row)) == nrow(L_fungi_row))
fungi_row <- apply(L_fungi_row, 2, function(x) sum(x, na.rm= TRUE))
fungi_row[L_fungi_row_na] <- NA
# L <- L[!row.names(L) %in% fungi,]
# L <- L[,!colnames(L) %in% fungi]
# Grouping herbaceous
L_herbaceous_col <- L[,herbaceous]
L_herbaceous_col_na <- which(rowSums(is.na(L_herbaceous_col)) == ncol(L_herbaceous_col))
herbaceous_col <- apply(L_herbaceous_col, 1, function(x) sum(x, na.rm = TRUE))
herbaceous_col[L_herbaceous_col_na] <- NA
L_herbaceous_row <- L[herbaceous,]
L_herbaceous_row_na <- which(colSums(is.na(L_herbaceous_row)) == nrow(L_herbaceous_row))
herbaceous_row <- apply(L_herbaceous_row, 2, function(x) sum(x, na.rm= TRUE))
herbaceous_row[L_herbaceous_row_na] <- NA
# L <- L[!row.names(L) %in% herbaceous,]
# L <- L[,!colnames(L) %in% herbaceous]
# Grouping invertebrates
L_invertebrates_col <- L[,invertebrates]
L_invertebrates_col_na <- which(rowSums(is.na(L_invertebrates_col)) == ncol(L_invertebrates_col))
invertebrates_col <- apply(L_invertebrates_col, 1, function(x) sum(x, na.rm = TRUE))
invertebrates_col[L_invertebrates_col_na] <- NA
L_invertebrates_row <- L[invertebrates,]
L_invertebrates_row_na <- which(colSums(is.na(L_invertebrates_row)) == nrow(L_invertebrates_row))
invertebrates_row <- apply(L_invertebrates_row, 2, function(x) sum(x, na.rm= TRUE))
invertebrates_row[L_invertebrates_row_na] <- NA
# L <- L[!row.names(L) %in% invertebrates,]
# L <- L[,!colnames(L) %in% invertebrates]
# Grouping reptilia
L_reptilia_col <- L[,reptilia]
L_reptilia_col_na <- which(rowSums(is.na(L_reptilia_col)) == ncol(L_reptilia_col))
reptilia_col <- apply(L_reptilia_col, 1, function(x) sum(x, na.rm = TRUE))
reptilia_col[L_reptilia_col_na] <- NA
L_reptilia_row <- L[reptilia,]
L_reptilia_row_na <- which(colSums(is.na(L_reptilia_row)) == nrow(L_reptilia_row))
reptilia_row <- apply(L_reptilia_row, 2, function(x) sum(x, na.rm= TRUE))
reptilia_row[L_reptilia_row_na] <- NA
# L <- L[!row.names(L) %in% reptilia,]
# L <- L[,!colnames(L) %in% reptilia]
# Grouping rodentia
L_rodentia_col <- L[,rodentia]
L_rodentia_col_na <- which(rowSums(is.na(L_rodentia_col)) == ncol(L_rodentia_col))
rodentia_col <- apply(L_rodentia_col, 1, function(x) sum(x, na.rm = TRUE))
rodentia_col[L_rodentia_col_na] <- NA
L_rodentia_row <- L[rodentia,]
L_rodentia_row_na <- which(colSums(is.na(L_rodentia_row)) == nrow(L_rodentia_row))
rodentia_row <- apply(L_rodentia_row, 2, function(x) sum(x, na.rm= TRUE))
rodentia_row[L_rodentia_row_na] <- NA
# L <- L[!row.names(L) %in% rodentia,]
# L <- L[,!colnames(L) %in% rodentia]
# Grouping trees
L_trees_col <- L[,trees]
L_trees_col_na <- which(rowSums(is.na(L_trees_col)) == ncol(L_trees_col))
trees_col <- apply(L_trees_col, 1, function(x) sum(x, na.rm = TRUE))
trees_col[L_trees_col_na] <- NA
L_trees_row <- L[trees,]
L_trees_row_na <- which(colSums(is.na(L_trees_row)) == nrow(L_trees_row))
trees_row <- apply(L_trees_row, 2, function(x) sum(x, na.rm= TRUE))
trees_row[L_trees_row_na] <- NA
# L <- L[!row.names(L) %in% trees,]
# L <- L[,!colnames(L) %in% trees]

# rbind and cbind the new groups
L <- as.data.frame(L)
L_colnames <- colnames(L)
L <- cbind(L,arthropoda_col,aves_col,Bryophyta_hepatics_lichens_col,detritus_col,fish_col,fungi_col,herbaceous_col,invertebrates_col,reptilia_col,rodentia_col,trees_col)
temp_mat <- as.data.frame(t(cbind(arthropoda_row,aves_row,Bryophyta_hepatics_lichens_row,detritus_row,fish_row,fungi_row,herbaceous_row,invertebrates_row,reptilia_row,rodentia_row,trees_row)))
L <- rbind.fill(L, temp_mat)
rownames(L) <- colnames(L)

L <- L[!row.names(L) %in% arthropoda,]
L <- L[,!colnames(L) %in% arthropoda]
L <- L[!row.names(L) %in% aves,]
L <- L[,!colnames(L) %in% aves]
L <- L[!row.names(L) %in% Bryophyta_hepatics_lichens,]
L <- L[,!colnames(L) %in% Bryophyta_hepatics_lichens]
L <- L[!row.names(L) %in% detritus,]
L <- L[,!colnames(L) %in% detritus]
L <- L[!row.names(L) %in% fish,]
L <- L[,!colnames(L) %in% fish]
L <- L[!row.names(L) %in% fungi,]
L <- L[,!colnames(L) %in% fungi]
L <- L[!row.names(L) %in% herbaceous,]
L <- L[,!colnames(L) %in% herbaceous]
L <- L[!row.names(L) %in% invertebrates,]
L <- L[,!colnames(L) %in% invertebrates]
L <- L[!row.names(L) %in% reptilia,]
L <- L[,!colnames(L) %in% reptilia]
L <- L[!row.names(L) %in% rodentia,]
L <- L[,!colnames(L) %in% rodentia]
L <- L[!row.names(L) %in% trees,]
L <- L[,!colnames(L) %in% trees]

View(colnames(L))
L <- rename(L, c("arthropoda_col"="Arthropoda","aves_col"="Aves","Bryophyta_hepatics_lichens_col"="Bryophyta","detritus_col"="Detritus","fish_col"="Fish","fungi_col"="Fungi","herbaceous_col"="Herbaceous","invertebrates_col"="Invertebrates","reptilia_col"="Reptilia","rodentia_col"="Rodentia","trees_col"="Trees"))
rownames(L) <- colnames(L)
L[L > 1 ] <- 1

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
