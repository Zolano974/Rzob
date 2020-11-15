#Matta

#multigraph

#prerequisite: importer le fichier Excel (.xslx) dans R, sous le nom de test1.xlsx
# install.packages("data.table")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("gridExtra")
#on charge les donnÃ©es 
library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
#for multigraph vizu
library(gridExtra)
library(cowplot)

setDT(ReccueilR)

source("utils/fn_matta.R")

mydata = ReccueilR[
  !is.na(Matta)
  &!is.na(Chirurgien)
]

dataset = ReccueilR[
  !is.na(Matta)
  &!is.na(Chirurgien)
]


#score de matta en fonction de l'age du chirurgien
p0 = generate_histogram_mata_expchir(mydata)
# p0

# score de matta en fonction du type de fracture
p1 = generate_histogram_mata_fracture(mydata)
# p1

p0
p1
