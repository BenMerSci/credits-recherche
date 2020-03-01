#Load the wide matrix
L <- readRDS("R/matrix_inter_wide.RDS")

#Load the list of species from all Quebec to check which species in our metaweb is missing
qc_list <- read.csv("R/liste_sp_qc.csv", header = T)

#Get the scientific names of the pred
pred_scientific_taxo <- as.character(qc_list[,"scientific_name"])

#Transpose the matrix so the pred are in the rows and get their names for comparison to see which species in our metaweb is in the list of the entire species of Quebec
L <- t(L)
L_names <- rownames(L)

#Which name of the metaweb are not in the species list
diff_name <- L_names[!(L_names %in% pred_scientific_taxo)]

#Manually combining duplicate preds in the matrix that are the same species but under two different latin name (old and new). So combining their two rows of interaction
#Moose
which(rownames(L) == "Alces alces")
which(rownames(L) == "Alces americanus")
temp <- rbind(L["Alces alces",], L["Alces americanus",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Alces americanus",] <- temp[3,]
L <- L[-8,]
rm(temp)

#Alopex/Vulpes lagopus
which(rownames(L) == "Alopex lagopus")
which(rownames(L) == "Vulpes lagopus")
temp <- rbind(L["Alopex lagopus",], L["Vulpes lagopus",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Vulpes lagopus",] <- temp[3,]
L <- L[-9,]
rm(temp)

#Anser/Chen caerulescens sub.sp atlantica
which(rownames(L)=="Anser caerulescens")
which(rownames(L)=="Chen caerulescens")
which(rownames(L) == "Anser caerulescens atlantica")
which(pred_scientific_taxo == "Anser caerulescens atlantica")
which(pred_scientific_taxo == "Anser caerulescens")
which(pred_scientific_taxo == "Chen caerulescens")

temp <- rbind(L["Anser caerulescens",], L["Anser caerulescens atlantica",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Anser caerulescens",] <- temp[3,]
L <- L[-19,]

temp_names <- rownames(L)
which(temp_names == "Anser caerulescens")
temp_names[18] <- "Chen caerulescens"
rownames(L) <- temp_names
rm(temp, temp_names)

#Cirucs hudsonius/cyaneus
which(rownames(L) == "Circus hudsonius")
which(rownames(L) == "Circus cyaneus")
temp <- rbind(L["Circus hudsonius",], L["Circus cyaneus",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Circus cyaneus",] <- temp[3,]
L <- L[-73,]
rm(temp)

#Dicrostonyx hudsonius/groenlandicus
which(rownames(L) == "Dicrostonyx hudsonius")
which(rownames(L) == "Dicrostonyx groenlandicus")
temp <- rbind(L["Dicrostonyx hudsonius",], L["Dicrostonyx groenlandicus",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Dicrostonyx hudsonius",] <- temp[3,]
L <- L[-100,]
rm(temp)

# Dyobates/Picoides pubescens
which(rownames(L) == "Dryobates pubescens")
which(rownames(L) == "Picoides pubescens")

temp <- rbind(L["Dryobates pubescens",], L["Picoides pubescens",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Picoides pubescens",] <- temp[3,]
L <- L[-102,]
rm(temp)

# Every Falco peregrinus (with no subspecies) in the metaweb are either from the dataset from Bylot or Nunavik. So I think it is reasonable to switch all the Falco peregrinus (with nob subspecies) to Falco peregrinus tundrius. and keep all the other ones that are way southern to Falco peregrinus anatum.
which(pred_scientific_taxo == "Falco peregrinus tundrius")
which(pred_scientific_taxo == "Falco peregrinus anatum")
which(pred_scientific_taxo == "Falco peregrinus")
which(rownames(L) == "Falco peregrinus")
which(rownames(L) == "Falco peregrinus tundrius")
which(rownames(L) == "Falco peregrinus anatum")

temp_names <- rownames(L)
which(temp_names == "Falco peregrinus")
temp_names[113] <- "Falco peregrinus tundrius"
rownames(L) <- temp_names
rm(temp_names)

# Removing Homo sapiens from the dataset
which(rownames(L) == "Homo sapiens")
L <- L[-126,]

# Lemnus trimucronatus -> Lemmus trimucronatus changing the n for a m in Lemnus/Lemmus
which(rownames(L) == "Lemmus trimucronatus") #none so i can just change it, no need to combine 2 rows
temp_names <- rownames(L)
which(temp_names == "Lemnus trimucronatus")
temp_names[143] <- "Lemmus trimucronatus"
rownames(L) <- temp_names
rm(temp_names)

#Leuconotopicus/Picoides villosus
which(rownames(L) == "Leuconotopicus villosus")
which(rownames(L) == "Picoides villosus")
temp <- rbind(L["Leuconotopicus villosus",], L["Picoides villosus",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Picoides villosus",] <- temp[3,]
L <- L[-146,]
rm(temp)

#Anas/Mareca americana
which(rownames(L) == "Anas americana")
which(rownames(L) == "Mareca americana")
temp_names <- rownames(L)
which(temp_names == "Mareca americana")
temp_names[157] <- "Anas americana"
rownames(L) <- temp_names
rm(temp_names)

#Melanitta americana/nigra
which(rownames(L) == "Melanitta americana")
temp_names <- rownames(L)
which(temp_names == "Melanitta americana")
temp_names[164] <- "Melanitta nigra"
rownames(L) <- temp_names
rm(temp_names)

#Parkesia/Seiurus noveboracensis
which(rownames(L) == "Seiurus noveboracensis")
which(rownames(L) == "Parkesia noveboracensis")
temp <- rbind(L["Seiurus noveboracensis",], L["Parkesia noveboracensis",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Parkesia noveboracensis",] <- temp[3,]
L <- L[-251,]
rm(temp)

#Poecile/Parus atricapillus
which(rownames(L) == "Parus atricapillus")
which(rownames(L) == "Poecile atricapillus")
temp <- rbind(L["Parus atricapillus",], L["Poecile atricapillus",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Poecile atricapillus",] <- temp[3,]
L <- L[-199,]
rm(temp)

##Poecile/Parus hudsonicus
which(rownames(L) == "Parus hudsonicus")
which(rownames(L) == "Poecile hudsonicus")
temp_names <- rownames(L)
which(temp_names == "Parus hudsonicus")
temp_names[199] <- "Poecile hudsonicus"
rownames(L) <- temp_names
rm(temp_names)

#
which(rownames(L) == "Rana pipiens")
which(rownames(L) == "Lithobates pipiens")
temp <- rbind(L["Rana pipiens",], L["Lithobates pipiens",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Lithobates pipiens",] <- temp[3,]
L <- L[-239,]
rm(temp)

#
which(rownames(L) == "Dendroica petechia")
which(rownames(L) == "Setophaga petechia")
temp <- rbind(L["Dendroica petechia",], L["Setophaga petechia",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Setophaga petechia",] <- temp[3,]
L <- L[-95,]
rm(temp)

#
which(rownames(L) == "Carduelis tristis")
which(rownames(L) == "Spinus tristis")
temp <- rbind(L["Carduelis tristis",], L["Spinus tristis",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Spinus tristis",] <- temp[3,]
L <- L[-57,]
rm(temp)

#
which(rownames(L) == "Zapus hudsonius")
which(rownames(L) == "Zapus hudsonicus")
temp <- rbind(L["Zapus hudsonius",], L["Zapus hudsonicus",])
temp <- rbind(temp, apply(temp, 2, sum, na.rm = TRUE))
for(i in 1:ncol(temp)){
  if(temp[3,i] > 1){temp[3,i] = 1}
  if(length(na.omit(temp[(1:2),i])) == 0){temp[3,i] = NA}
}
L["Zapus hudsonius",] <- temp[3,]
L <- L[-306,]
rm(temp)

#Write the last version matrix
write_rds(L, "R/final_matrix_inter_wide.RDS")
rm(list=ls())

#Get the taxonomic hierarchy for each species for a dsitance measure in the KNN SCRIPT
L <- readRDS("R/final_matrix_inter_wide.RDS")
pred_scientific_taxo <- rownames(L)
taxo_rank <- classification(pred_scientific_taxo, db = "gbif", row = 1)

#Get the species for which we don't have the taxonomic hierarchy
vect_name <- c()
for(i in 1:length(taxo_rank)){
  if(nrow(as.data.frame(taxo_rank[i]))<2){vect_name <- append(vect_name, names(taxo_rank[i]))}
} #Looks like there isn't any species for which we weren't able to rebuild their taxonomic hierarchy

write_rds(taxo_rank, "R/pred_phylogeny.RDS")
rm(list=ls())
