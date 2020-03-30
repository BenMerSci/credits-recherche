# Setting the WD and libraries needed
setwd("/home/benjamin/Documents/credits_recherche")
library(sf)
library(raster)
library(rgdal)
library(plyr)
# Load the matrix and list of qc species to get their name and subset the range maps
L_allQc <- readRDS("data/intermediate_object_results/matrix_inter_wide_all.RDS")
#qc_list <- read.csv("data/intermediate_object_results/liste_sp_qc.csv", header = TRUE)
# Get the names of our metaweb species and qc species
sp_L_allQc <- rownames(L_allQc)
#sp_qc_list <- as.character(qc_list$scientific_name)

#### SECTION FOR GLOBAL DOWNLOAD OF EACH TAXON ####
#full_bird <- readOGR("data/range_maps/BOTW/BOTW.gdb", layer = "All_Species", require_geomType = "wkbPolygon")
#full_bird <- st_read("data/range_maps/BOTW/BOTW.gdb", layer = "All_Species", stringAsFactors = FALSE)
full_amphi <- st_read("data/range_maps/AMPHIBIANS.shp", stringsAsFactors = FALSE )

full_mamm <- st_read("data/range_maps/TERRESTRIAL_MAMMALS.shp", stringsAsFactors = FALSE)

full_rept <- st_read("data/range_maps/REPTILES.shp", stringsAsFactors = FALSE)
sub_rept <- st_read("data/range_maps/data_0.shp") #for Emydoidea blandingii
sub_rept <- sub_rept[-1,-1]
temp_df <- as.data.frame(matrix(nrow = nrow(sub_rept), ncol = 13), stringsAsFactors = FALSE)
colnames(temp_df) <- c("source","kingdom","phylum","class","order_","family","genus","category","marine","terrestrial","freshwater","SHAPE_Leng","SHAPE_Area")
class(temp_df) <- class(sub_rept)
sub_rept <- cbind(sub_rept, temp_df)
sub_rept <- sub_rept[,c(1,2,3,4,5,6,7,8,15,12,13,10,11,9,14,16,17,18,19,20,21,22,23,24,25,26,27,28)]
colnames(sub_rept) <- colnames(full_rept)
full_rept <- rbind(full_rept, sub_rept)

full_bird <- readOGR("data/range_maps/Clip_subQc_BOTW.shp", stringsAsFactors = FALSE) %>%
  st_as_sf(.x)
full_bird <- full_bird[,c(1,3,6,7,8,11,4,15,13,12,17,18,19)]
temp_df <- as.data.frame(matrix(nrow = nrow(full_bird), ncol = 15), stringsAsFactors = FALSE)
colnames(temp_df) <- c("source","island","subspecies","subpop","legend","kingdom","phylum","class","order_","family","genus","category","marine","terrestrial","freshwater")
class(temp_df) <- class(full_bird)
full_bird <- cbind(full_bird, temp_df)
full_bird <- full_bird[,c(1,2,3,4,5,6,7,8,13,9,14,15,16,17,10,18,19,20,21,22,23,24,25,26,27,11,12,28)]
colnames(full_bird) <- colnames(full_amphi)

# Subsetting the maps for the species that we have in Quebec (in our model)
#birds
sub_birdQc <- full_bird[which(full_bird$binomial %in% sp_L_allQc),]
#amphibian
sub_amphiQc <- full_amphi[which(full_amphi$binomial %in% sp_L_allQc),]
#reptilia
sub_reptQc <- full_rept[which(full_rept$binomial %in% sp_L_allQc),]
#mammalia
sub_mammQc <- full_mamm[which(full_mamm$binomial %in% sp_L_allQc),]
# Get all the taxon into one data frame
all_sp <- rbind(sub_amphiQc,sub_reptQc,sub_mammQc, sub_birdQc)

# Load the Quebec map
qc_map <- readRDS(file = "data/range_maps/Quebec_Labrador.RDS")
# Construire un raster de référence
rasterBase <-  raster(qc_map, resolution = c(0.01,0.01))
# Transformer une carte en raster
# rasterQc <-  rasterize(qc_map, rasterBase)


rm(list=(setdiff(ls(), c("all_sp","rasterBase"))))
# Only keep presence=1 (means extant, 2=probably extent, 3=possibly extant, 4=possibly extinct)
# Keep all the code for "Origin", (1=native, 2=reintroduced, 3=introduced, 4=vagrant, 5=origin uncertain, 6=assisted colinisation)
# Keep all the code for "Seasonality", (1=resident, 2=breeding season, 3=non-breeding season, 4=passage, 5=seasonal occurence uncertain)
all_sp <- all_sp[which(all_sp$presence == 1),]

# Split the data frame into lists based on the binomial name. (some species had more than one geometry associated) SO going to merge these geometry together
all_sp <- split(all_sp, f = all_sp$binomial)
# Combine for each species their multi-multi polygon into one polygon. (My guess is it's faster to rasterize?)
all_sp <- lapply(all_sp, function(x) st_as_sf(st_combine(x)))
# Rasterize each layer, have to do it overnight. Creates a large list of raster of 4.3 Gb!
#all_sp_raster <- lapply(all_sp, function(x) rasterize(x, rasterBase))
#sp_stack <- stack(all_sp_raster)
#writeRaster(sp_stack, "data/intermediate_object_results/sp_stack.grd", format = "raster")
rm(list=(ls()))

#################################################################################################################################################################
########################################################### "SPATIAL" ANALYSIS TIME MY DUDE #####################################################################
#################################################################################################################################################################
library(stringr)
library(tidyverse)
# Read the stack
#sp_stack <- stack("data/intermediate_object_results/sp_stack.grd")
# Read the matrices
L <- t(readRDS("data/intermediate_object_results/matrix_inter_wide.RDS"))
L_infered_k5 <- t(readRDS("data/intermediate_object_results/infered_matrix_metanetwork_k5"))
L_allQc <- t(readRDS("data/intermediate_object_results/matrix_inter_wide_all.RDS"))
L_allQc_infered_k5 <- t(readRDS("data/intermediate_object_results/infered_matrix_allQc_k5"))
# Read the Qc map to make the raster of the new general preys that will be on all the territory
# Load the Quebec map
qc_map <- readRDS(file = "data/range_maps/Quebec_Labrador.RDS")
# Construire un raster de référence
rasterBase <-  raster(qc_map, resolution = c(0.01,0.01))
#Arthropoda <- rasterize(qc_map, rasterBase)
#Aves <- rasterize(qc_map, rasterBase)
#Bryophyta <- rasterize(qc_map, rasterBase)
#Detritus <- rasterize(qc_map, rasterBase)
#Fish <- rasterize(qc_map, rasterBase)
#Fungi <- rasterize(qc_map, rasterBase)
#Herbaceous <- rasterize(qc_map, rasterBase)
#Invertebrates <- rasterize(qc_map, rasterBase)
#Reptilia <- rasterize(qc_map, rasterBase)
#Rodentia <- rasterize(qc_map, rasterBase)
#Trees <- rasterize(qc_map, rasterBase)
#sp_stack_new <- stack(Arthropoda,Aves,Bryophyta,Detritus,Fish,Fungi,Herbaceous,Invertebrates,Reptilia,Rodentia,Trees)
#sp_stack_new <- stack(sp_stack, sp_stack_new)
#writeRaster(sp_stack_new, "data/intermediate_object_results/sp_stack_new.grd", format = "raster")
sp_stack_new <- stack("data/intermediate_object_results/sp_stack_new.grd")
# Change the row/colnames of matrices to fit with the names in the stack
rownames(L_allQc) <- str_replace(rownames(L_allQc), " ", ".")
colnames(L_allQc) <- str_replace(colnames(L_allQc), " ", ".")


raster_test <- rasterize(qc_map, rasterBase)
vect_test <- c(rep(NA, ncell(raster_test)))
vect_test[1] <- 1
raster_test <- setValues(raster_test, vect_test)
raster_NS <- rasterize(qc_map, rasterBase)
raster_NProd <- rasterize(qc_map, rasterBase)
raster_NInt <- rasterize(qc_map, rasterBase)
raster_NTop <- rasterize(qc_map, rasterBase)
raster_NL <- rasterize(qc_map, rasterBase)
raster_C <- rasterize(qc_map, rasterBase)
raster_GenSD <- rasterize(qc_map, rasterBase)
raster_VulSD <- rasterize(qc_map, rasterBase)
raster_MeanTR <- rasterize(qc_map, rasterBase) 
raster_MaxTR <- rasterize(qc_map, rasterBase)
raster_Omn <- rasterize(qc_map, rasterBase)
stats_list <- list()
vect_NS <- c()
vect_NProd <- c()
vect_NInt <- c()
vect_NTop <- c()
vect_NL <- c()
vect_C <- c()
vect_GenSD <- c()
vect_VulSD <- c()
vect_MeanTR <- c()
vect_MaxTR <- c()
vect_Omn <- c()
#test <- raster(nrows = nrow(sp_stack), ncols = ncol(sp_stac  k), ext = extent(sp_stack), crs = proj4string(sp_stack))
system.time(rasterMetric_allQc <- for(i in 1:nrow(sp_stack_new)){
  for(j in 1:ncol(sp_stack_new)){
    temp_df <- data.frame(sp_stack_new[i,j]) # Take the list of species from on pixel (cell)
    sub_names <- colnames(temp_df[which(temp_df == 1)]) # Get only the species which are present (1) in that cell
    sub_names <- colnames(L_allQc)[colnames(L_allQc) %in% sub_names] # Get their names
    sub_matrix <- L_allQc[sub_names,sub_names] # Subset the interaction matrix with only the present species in that cell
    stats_list <- stats_fn(sub_matrix) # Calculate all the matrix statistics on that sub_matrix
    #raster_NS[i,j] <- stats_list["NS"] # associate each  statstics to their appropriate layer
    #raster_NProd[i,j] <- stats_list["NProd"] # associate each  statstics to their appropriate layer
    #raster_NInt[i,j] <- stats_list["NInt"] # associate each  statstics to their appropriate layer
    #raster_NTop[i,j] <- stats_list["NTop"] # associate each  statstics to their appropriate layer
    #raster_NL [i,j] <- stats_list["NL"] # associate each  statstics to their appropriate layer
    #raster_C[i,j] <- stats_list["C"] # associate each  statstics to their appropriate layer
    #raster_GenSD[i,j] <- stats_list["GenSD"] # associate each  statstics to their appropriate layer
    #raster_VulSD[i,j] <- stats_list["VUlSD"] # associate each  statstics to their appropriate layer
    #raster_MeanTR[i,j] <- stats_list["MeanTR"] # associate each  statstics to their appropriate layer
    #raster_MaxTR[i,j] <- stats_list["MaxTR"] # associate each  statstics to their appropriate layer
    #raster_Omn[i,j] <- stats_list["Omn"]
    vect_NS <- append(vect_NS, stats_list["NS"])
    vect_NProd <- append(vect_NProd, stats_list["NProd"])
    vect_NInt <- append(vect_NInt, stats_list["NInt"])
    vect_NTop <- append(vect_NTop, stats_list["NTop"])
    vect_NL <- append(vect_NL, stats_list["NL"])
    vect_C <- append(vect_C, stats_list["C"])
    vect_GenSD <- append(vect_GenSD, stats_list["GenSD"])
    vect_VulSD <- append(vect_VulSD, stats_list["VUlSD"])
    vect_MeanTR <- append(vect_MeanTR, stats_list["MeanTR"])
    vect_MaxTR <- append(vect_MaxTR, stats_list["MaxTR"])
    vect_Omn <- append(vect_Omn, stats_list["Omn"])
  }
} 
)


    # dans chaque cellule cest quoi mes especes, prendre matrice 
# sauver des pointeurs de ligne et colone, pour chaque cellule: sauvegarder dans list index de ligne/colonne
#  dans boucle : met if de sauvgarde a chaque mille cellule 





