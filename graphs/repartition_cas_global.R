#répartition globale des cas

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

source("utils/fn_cas.R")

setDT(ReccueilR)


fulldata = ReccueilR


# -> répartition des cas par type de trauma

p1 = generate_plot_patients_repartition_by_traumatisme(fulldata)
p1

# -> répartition des cas par type de fracture

p2 = generate_plot_patients_repartition_by_fracture(fulldata)
p2

# -> repartition des cas par voie d'abord 
p2bis = generate_plot_patients_repartition_by_voie_abord(fulldata)
p2bis

# -> répartition des cas par XP chirurgien
p3 = generate_plot_patients_repartition_by_xp_chir(fulldata)
p3

# -> répartition des cas par score MATTA
p4 = generate_plot_patients_repartition_by_matta(fulldata)
p4
