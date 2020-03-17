library(sf)
setwd("/home/benjamin/Documents/credits_recherche")
library(sf)
# Load the matrix and list of qc species to get their name and subset the range maps
L <- readRDS("R/final_matrix_inter_wide.RDS")
qc_list <- read.csv("R/liste_sp_qc.csv", header = TRUE)
# Get the names of our metaweb species and qc species
sp_L <- rownames(L)
sp_qc_list <- as.character(qc_list$scientific_name)

#### SECTION FOR GLOBAL DOWNLOAD OF EACH TAXON ####
full_amphi <- st_read("data/range_maps/AMPHIBIANS.shp")
full_rept <- st_read("data/range_maps/REPTILES.shp")
full_mamm <- st_read("data/range_maps/TERRESTRIAL_MAMMALS.shp")
sub_rept <- st_read("data/range_maps/data_0.shp") #for Emydoidea blandingii
#amphi
full_amphi_sub_qc_list <- full_amphi[which(full_amphi$binomial %in% sp_qc_list),]
full_amphi_sub_qc_list_name <- unique(as.character(full_amphi_sub_qc_list$binomial))


#rept
full_rept_sub_qc_list <- full_rept[which(full_rept$binomial %in% sp_qc_list),]
full_rept_sub_qc_list_name <- unique(as.character(full_rept_sub_qc_list$binomial))


#mamm
full_mamm_sub_qc_list <- full_mamm[which(full_mamm$binomial %in% sp_qc_list),]
full_mamm_sub_qc_list_name <- unique(as.character(full_mamm_sub_qc_list$binomial))



# #### SECTION FOR SUBSET DOWNLOAD FROM CRITERIA ####
# sub_amphi <- st_read("data/range_maps/qc_amphi.shp")
# sub_rept <- st_read("data/range_maps/qc_reptiles.shp")
# sub_mamm <- st_read("data/range_maps/qc_mamm.shp")
# 
# #amphi
# sub_amphi_sub_qc_list <- sub_amphi[which(sub_amphi$BINOMIAL %in% sp_qc_list),]
# sub_amphi_sub_qc_list_name <- unique(as.character(sub_amphi_sub_qc_list$BINOMIAL)) #got them all, but less than uptop
# sub_amphi_sub_metaweb <- sub_amphi[which(sub_amphi$BINOMIAL %in% sp_L),]
# sub_amphi_sub_metaweb_name <- unique(as.character(sub_amphi_sub_metaweb$BINOMIAL))
# 
# #rept
# sub_rept_sub_qc_list <- sub_rept[which(sub_rept$BINOMIAL %in% sp_qc_list),]
# sub_rept_sub_qc_list_name <- unique(as.character(sub_rept_sub_qc_list$BINOMIAL)) #got them all, but less than uptop
# sub_rept_sub_metaweb <- sub_rept[which(sub_rept$BINOMIAL %in% sp_L),]
# sub_rept_sub_metaweb_name <- unique(as.character(sub_rept_sub_metaweb$BINOMIAL))
# 
# #mamm
# sub_mamm_sub_qc_list <- sub_mamm[which(sub_mamm$BINOMIAL %in% sp_qc_list),]
# sub_mamm_sub_qc_list_name <- unique(as.character(sub_mamm_sub_qc_list$BINOMIAL)) #got them all, but less than uptop
# sub_mamm_sub_metaweb <- sub_mamm[which(sub_mamm$BINOMIAL %in% sp_L),]
# sub_mamm_sub_metaweb_name <- unique(as.character(sub_mamm_sub_metaweb$BINOMIAL))

