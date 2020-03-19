##libraries
library(sf)
library(taxize)
library(traits)
library(dplyr)
library(purrr)

rm("apg_families","apg_orders","rank_ref","theplantlist")
#set the working directory
setwd("~/Documents/credits_recherche/")

#read the shp of all chordata distribution
#poly_chord <- st_read(".", layer =)
poly_chord <- read_sf("chordata_qc_polygon/data_0.shp")
summary(poly_chord)

#get the names of every chord data recorded
chord_names <- unique(poly_chord$BINOMIAL)
chord_names

output <- matrix(ncol=7, nrow=length(chord_names)) #matrix to store the data, will be transform into df

for(x in 1:length(chord_names)){
temp_vect_data <- c()
for(i in chord_names[x]){
temporary <- traitbank(query = paste("MATCH (t:Trait)<-[:trait]-(p:Page),
(t)-[:predicate]->(pred:Term)
WHERE p.canonical =", paste0("'", i, "'"), "AND pred.name = 'body mass'
OPTIONAL MATCH (t)-[:lifestage_term]->(lifestage:Term)
OPTIONAL MATCH (t)-[:units_term]->(units:Term)
RETURN p.canonical, pred.name, t.measurement, units.name
LIMIT 10"), key = Sys.getenv("EOL_TRAITS_KEY"))
if(length(temporary[[2]])>0) {temporary[[2]][,3] <- round(as.numeric(temporary[[2]][,3]), digits = 3); temp_vect_data <- temporary[[2]][row(temporary[[2]])[temporary[[2]]==max(as.numeric(temporary[[2]][,3]))] , ,drop=F][1,]} else {temp_vect_data <- c(chord_names[x],NA,NA,NA)}

temporary <- traitbank(query = paste("MATCH (t:Trait)<-[:trait]-(p:Page),
(t)-[:predicate]->(pred:Term)
WHERE p.canonical =", paste0("'", i, "'"), "AND pred.name = 'trophic level' 
OPTIONAL MATCH (t)-[:object_term]->(obj:Term)
RETURN pred.name, obj.name, p.page_id
LIMIT 1"), key = Sys.getenv("EOL_TRAITS_KEY"))
if(length(temporary[[2]])>0) {temp_vect_data <- as.vector(unlist(append(temp_vect_data, temporary[[2]])))} else {temporary <- c(NA,NA,NA); temp_vect_data <- as.vector(unlist(append(temp_vect_data, temporary)))}
  }
output[x,] <- temp_vect_data
}

##### Check if the ones I don't have data for are because of error in taxonomy ####
test <- as.data.frame(output) %>% 
        filter_all(any_vars(is.na(.)))
        gnr_resolve(as.vector(as.character(test[,1])), resolve_once = T, with_context = T, canonical = T, best_match_only = T)
        
        

##### TEST to put the 2 EOL traits Cypher queries into one #### which isn't working ####
# traitbank(query = paste("MATCH (t:Trait)<-[:trait]-(p:Page),
# (t)-[:predicate]->(pred:Term),
# (t1:Trait)<-[:trait]-(p:Page),
# (t1)-[:predicate]->(pred:Term)
# WHERE p.canonical =", paste0("'", i, "'"), "AND pred.name = 'body mass' AND pred.name1 = 'trophic level'
# OPTIONAL MATCH (t)-[:units_term]->(units:Term)
# OPTIONAL MATCH (t1)-[:object_term]->(obj:Term)
# RETURN p.canonical, pred.name, t.measurement, units.name, pred.name1, obj.name, p.page_id
# LIMIT 50"), key = Sys.getenv("EOL_TRAITS_KEY"))








        
