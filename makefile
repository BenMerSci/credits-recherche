# THE knn algorithm
data/intermediate_object_results/infered_matrix_metanetwork.RDS data/intermediate_object_results/infered_matrix_allQc.RDS : knn_algo.R
	Rscript R/knn_algo.R

# Calculate the distance matrices (interactions and phylogenetic)
data/intermediate_object_results/dist_mat_metanetwork.RDS data/intermediate_object_results/dist_mat_allQc.RDS : R/distance_matrices.R
	Rscript R/distance_matrices.R

# Reconstruct the phylogeny of each species in the model
data/intermediate_object_results/pred_phylogeny.RDS data/intermediate_object_results/pred_phylogeny_miss.RDS : R/reconstruct_phylogeny.R
	Rscript R/reconstruct_phylogeny.R

# Creating the base interaction matrix
data/intermediate_object_results/matrix_inter_long.RDS data/intermediate_object_results/matrix_inter_wide.RDS data/intermediate_object_results/matrix_inter_wide_all.RDS data/intermediate_object_results/sp_outof_fw.RDS : R/creating_inter_dictionnary.R
	Rscript R/creating_inter_dictionnary.R

# Uniformisation des données de base
data/Cohen_ecoweb/web23.csv data/Cohen_ecoweb/web24.csv data/Cohen_ecoweb/web25.csv data/Cohen_ecoweb/web26.csv : R/Formatting_scripts_and_functions/bird1930_formatting.R R/Formatting_scripts_and_functions/sep_pooled_many.R
	Rscript R/Formatting_scripts_and_functions/bird1930_formatting.R

data/Cohen_ecoweb/web59.csv : R/Formatting_scripts_and_functions/twomey1945_formatting.R R/Formatting_scripts_and_functions/sep_pooled.R
	Rscript R/Formatting_scripts_and_functions/twomey1945_formatting.R 

data/legagneux_2014/interactions_alert_bylot.csv : R/Formatting_scripts_and_functions/legagneux2014_formatting.R
	Rscript R/Formatting_scripts_and_functions/legagneux2014_formatting.R

data/leroux_2014/leroux_2014.csv : R/Formatting_scripts_and_functions/leroux2014_formatting.R
	Rscript R/Formatting_scripts_and_functions/leroux2014_formatting.R

data/MSH_2010/MSH_food_web_original/MSH_food_web_original/1_Working files/interactions_msh.csv : R/Formatting_scripts_and_functions/MSH_formatting.R
	Rscript R/Formatting_scripts_and_functions/MSH_formatting.R

data/Ropars_nordic_2018/ds_000582043/interactions_nunavik.csv : R/Formatting_scripts_and_functions/nunavik_formatting.R
	Rscript R/Formatting_scripts_and_functions/nunavik_formatting.R
