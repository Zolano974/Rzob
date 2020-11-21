#Type de Fracture en fonction du type de Traumatisme

#prerequisite: importer le fichier Excel (.xslx) dans R, sous le nom de test1.xlsx
install.packages(data.table)
install.packages(stringr)
install.packages(ggplot2)
install.packages(dplyr)
#on charge les donnÃ©es 
library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

source("utils/fn_fractures.R")
source("utils/fn_count.R")

setDT(ReccueilR)

# ----------------------------------------- #
# I -Fracture en fonction du type de trauma #
# ----------------------------------------- #

#filter the initial data
mydata1 = ReccueilR[!is.na(Traumatisme)&!is.na(Fracture)]

#generate plot
fractureEnFonctionDuTrauma = generate_plot_fracture_by_traumatisme(mydata1)
fractureEnFonctionDuTrauma

# ----------------------------------------- #
# I -Fracture en fonction de l'age          #
# ----------------------------------------- #

mydata2 = ReccueilR[!is.na(Age)&!is.na(Fracture)]

fractureEnFonctionDeLage = generate_plot_fracture_by_age(mydata2)
fractureEnFonctionDeLage

# ----------------------------------------- #
# I -Fracture en fonction de l'xp chir      #
# ----------------------------------------- #

mydata3 = ReccueilR[!is.na(Chirurgien)&!is.na(Fracture)]
fracture_x_chir = generate_histogram_fracture_expchir(mydata3)
fracture_x_chir


# my_sandbox()




