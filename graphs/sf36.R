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

calculate_sf36_means(sf36)
generate_SF36_circularo_barplot(sf36)

# ------------------------------------------- #
# Moyenne des notes SF36 par type de fracture #
# ------------------------------------------- #

#paroi post
calculate_sf36_means(sf36[typefracture == "paroi post"])
generate_SF36_circularo_barplot(sf36[typefracture == "paroi post"])

#transversale
calculate_sf36_means(sf36[typefracture == "transversale"])
generate_SF36_circularo_barplot(sf36[typefracture == "transversale"])

#transversale et paroipost
calculate_sf36_means(sf36[typefracture == "transversale et paroi post"])
generate_SF36_circularo_barplot(sf36[typefracture == "transversale et paroi post"])

#colonne antérieure
calculate_sf36_means(sf36[typefracture == "colonne antérieure"])
generate_SF36_circularo_barplot(sf36[typefracture == "colonne antérieure"])

#bi colonne
calculate_sf36_means(sf36[typefracture == "bi colonne"])
generate_SF36_circularo_barplot(sf36[typefracture == "bi colonne"])

#T
calculate_sf36_means(sf36[typefracture == "T"])
generate_SF36_circularo_barplot(sf36[typefracture == "T"])



        