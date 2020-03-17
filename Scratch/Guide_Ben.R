# Pour raster
library(raster)
library(sf)
setwd("/home/benjamin/Documents/credits_recherche")
# Lire polygon
QLSimple <- readRDS(file = "data/range_maps/Quebec_Labrador.RDS")
sub_mamm <- st_read("data/range_maps/TERRESTRIAL_MAMMALS.shp")
sub_mamm1 <- sub_mamm[c(1461,3664,5157), "geometry"]

# Construire un raster de référence
rasterBase <-  raster(QLSimple, resolution = c(0.01,0.01))

# Transformer une carte en raster
rasterQL <-  rasterize(QLSimple, rasterBase)
rasterCerf <- rasterize(sub_mamm1[1,], rasterBase)
rasterLepus <- rasterize(sub_mamm1[2,], rasterBase)
rasterCanis <- rasterize(sub_mamm1[3,], rasterBase)




plot(rasterQL, col = "cornsilk2", main = "Faible résolution", legend = "")
plot(rasterQL, col = "cornsilk2", main = "Haute résoltuion", legend = "")
plot(rasterLepus, add = TRUE, col = rgb(0.146, 0.209, 0.87, 0.5))
plot(rasterCerf, add = TRUE, col = rgb(0, 0.255, 0.127, 0.5))
plot(rasterCanis, add = TRUE, col = rgb(0.270, 0, 0.128, 0.5))
legend(-63, 60, legend=c("Lièvre arctique", "Coyote"),
       col=c(rgb(0.146, 0.209, 0.87, 0.5), rgb(0.270, 0, 0.128, 0.5)), pch = 19)
