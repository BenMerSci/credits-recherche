# Setting the WD and libraries needed
setwd("/home/benjamin/Documents/credits_recherche")
library(sf)
library(raster)
library(rgdal)

# Load the matrix and list of qc species to get their name and subset the range maps
L_allQc <- readRDS("data/intermediate_object_results/matrix_inter_wide_all.RDS")
#qc_list <- read.csv("data/intermediate_object_results/liste_sp_qc.csv", header = TRUE)
# Get the names of our metaweb species and qc species
sp_L_allQc <- rownames(L_allQc)
#sp_qc_list <- as.character(qc_list$scientific_name)

#### SECTION FOR GLOBAL DOWNLOAD OF EACH TAXON ####
ogrListLayers("data/range_maps/BOTW/BOTW.gdb")
full_bird <- readOGR("data/range_maps/BOTW/BOTW.gdb", layer = "All_Species", require_geomType = "wkbPolygon")
st_layers("data/range_maps/BOTW/BOTW.gdb")
full_bird <- st_read("data/range_maps/BOTW/BOTW.gdb", layer = "ALl_Species", stringAsFactors = FALSE)
full_amphi <- st_read("data/range_maps/AMPHIBIANS.shp", stringsAsFactors = FALSE )
full_rept <- st_read("data/range_maps/REPTILES.shp", stringsAsFactors = FALSE)
full_mamm <- st_read("data/range_maps/TERRESTRIAL_MAMMALS.shp", stringsAsFactors = FALSE)
#sub_rept <- st_read("data/range_maps/data_0.shp") #for Emydoidea blandingii


# Subsetting the maps for the species that we have in Quebec (in our model)
#amphibian
sub_amphiQc <- full_amphi[which(full_amphi$binomial %in% sp_L_allQc),]
#reptilia
sub_reptQc <- full_rept[which(full_rept$binomial %in% sp_L_allQc),]
#mammalia
sub_mammQc <- full_mamm[which(full_mamm$binomial %in% sp_L_allQc),]
# Get all the taxon into one data frame
all_sp <- rbind(sub_amphiQc,sub_reptQc,sub_mammQc)

# Only keep presence=1 (means extant, 2=probably extent, 3=possibly extant, 4=possibly extinct)
# Keep all the code for "Origin", (1=native, 2=reintroduced, 3=introduced, 4=vagrant, 5=origin uncertain, 6=assisted colinisation)
# Keep all the code for "Seasonality", (1=resident, 2=breeding season, 3=non-breeding season, 4=passage, 5=seasonal occurence uncertain)
all_sp <- all_sp[which(all_sp$presence == 1)]

# Split the data frame into lists based on the binomial name. (some species had more than one geometry associated) SO going to merge these geometry together
all_sp <- split(all_sp, f = all_sp$binomial)
all_sp_raster <- lapply(all_sp, function(x) rasterize(x, rasterBase))
#write_rds(all_sp_raster, "data/intermediate_object_results/all_sp_raster.RDS")



test <- raster::bind(all_sp[[1]])

test <- st_union(all_sp[[1]])








dz_species_gdb@ftp.birdlife.org
;[WgL;M{AW2l







image(rasterQc)
image(test, add = TRUE)
# Load the Quebec map
qc_map <- readRDS(file = "data/range_maps/Quebec_Labrador.RDS")
# Construire un raster de référence
rasterBase <-  raster(qc_map, resolution = c(0.01,0.01))
# Transformer une carte en raster
rasterQc <-  rasterize(qc_map, rasterBase)
# Plot la carte du Qc
plot(rasterQc, col = "cornsilk2", main = "Qc map")
# Ajouter la carte de l'sp1
plot(test, add = TRUE, col = rgb(0.146, 0.209, 0.87, 0.5), asp = -1)







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

