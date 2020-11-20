#répartition des résultats à chaque score, en sous groupe (type de fracture)

#prerequisite: importer le fichier Excel (.xslx) dans R, sous le nom de test1.xlsx
# install.packages("data.table")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages(dplyr)
#on charge les donnÃ©es 
library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)

source("utils/fn_fractures.R")

setDT(ReccueilR)

source("utils/fn_scores.R")

# ----------------------------------------- #
# I -Fracture en fonction du type de trauma #
# ----------------------------------------- #

#filter the initial data
fulldata = ReccueilR[
  !is.na(Oxford)
  &!is.na(PMA)
  &!is.na(HarrisHS)
  &!is.na(Womac)
  &!is.na(Fracture)
]



#keep only 4 scores + fracture type
scoresdata = filter_dataset_score_fracture(fulldata)
# View(scoresdata)

#STATS GLOBALE
global_stats_table = calculate_global_stat_by_score(scoresdata)
# View(global_stats_table)

#SOUS GROUPES PAR TYPE DE FRACTURE

#APPROCHE I -> 1 tableau pour chaque type de fracture
scores_t_data <- scoresdata[Fracture == "T"]
T_stats_table = calculate_global_stat_by_score(scores_t_data)
T_stats_table

bicolonne_data <- scoresdata[Fracture == "bi colonne"]
bicolonne_stats_table = calculate_global_stat_by_score(bicolonne_data)
bicolonne_stats_table

colonneanterieure_data <- scoresdata[Fracture == "colonne antérieure"]
colonneanterieure_stats_table = calculate_global_stat_by_score(colonneanterieure_data)
colonneanterieure_stats_table

paroiposterieure_data <- scoresdata[Fracture == "paroi postérieure"]
paroiposterieure_stats_table = calculate_global_stat_by_score(paroiposterieure_data)
paroiposterieure_stats_table

transversale_data <- scoresdata[Fracture == "transversale"]
transversale_stats_table = calculate_global_stat_by_score(transversale_data)
transversale_stats_table

transversale_paroipost_data <- scoresdata[Fracture == "transversale et paroi postérieure"]
transversale_paroipost_stats_table = calculate_global_stat_by_score(transversale_paroipost_data)
transversale_paroipost_stats_table

#Approche II -> un seul tableau

mean_by_score <- apply(scoresdata[,1:4],2,tapply, scoresdata$Fracture, mean)
# View(mean_by_score)

median_by_score <- apply(scoresdata[,1:4],2,tapply, scoresdata$Fracture, median)
# View(median_by_score)

#Boxplots

oxford_bp = generate_score_boxplot(scoresdata, "Oxford")

womac_bp = generate_score_boxplot(scoresdata, "Womac")
harris_bp = generate_score_boxplot(scoresdata, "HarrisHS")
pma_bp = generate_score_boxplot(scoresdata, "PMA")


plot_grid(oxford_bp, womac_bp, harris_bp, pma_bp, labels=c(NA, NA, NA, NA),ncol = 2, nrow = 2)




