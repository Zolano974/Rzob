library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(hash)
library(tidyverse)

source("utils/fn_sf36.R")

# fonctionphysique
# limitationphysique
# limitationemotionnelle
# energiefatigue
# bienetre
# fonctionsociale
# douleur
# santegenerale
# modificationsante
# typefracture

setDT(sf36)

# ------------------------------- #
# Moyennes des notes SF36 globale #
# ------------------------------- #

sf36_global_mean = calculate_sf36_means(sf36)
sf36_global_mean


# ------------------------------------------- #
# Moyenne des notes SF36 par type de fracture #
# ------------------------------------------- #

#paroi post
sf36_mean_paroipost = calculate_sf36_means(sf36[typefracture == "paroi post"])
sf36_mean_paroipost

#transversale
sf36_mean_transversale = calculate_sf36_means(sf36[typefracture == "transversale"])
sf36_mean_transversale

#transversale et paroipost
sf36_mean_transversale_paroipost = calculate_sf36_means(sf36[typefracture == "transversale et paroi post"])
sf36_mean_transversale_paroipost

#colonne antérieure
sf36_mean_colonne_antérieure = calculate_sf36_means(sf36[typefracture == "colonne antérieure"])
sf36_mean_colonne_antérieure

#bi colonne
sf36_mean_bicolonne = calculate_sf36_means(sf36[typefracture == "bi colonne"])
sf36_mean_bicolonne

#T
sf36_mean_T = calculate_sf36_means(sf36[typefracture == "T"])
sf36_mean_T




