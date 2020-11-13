#Type de Fracture en fonction de l'age

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

setDT(ReccueilR)


dFractureByAge = ReccueilR[!is.na(Age)&!is.na(Fracture)]

dFractureByAge = dFractureByAge[Fracture=="T", Fracture:= " T"]
dFractureByAge = dFractureByAge[Fracture=="colonne antérieure", Fracture:= " colonne antérieure"]

p2 = dFractureByAge %>%
  ggplot(aes(x=Age, fill=Fracture, order=Age)) +
  geom_histogram(
    binwidth=15,
    orientation="x",
    colour="white"
  ) +
  labs (
    title= "Répartition des fractures en fonction de l'âge",
    x="Classe d'âge",
    y="Nombre de fractures"
  ) +
  theme(
    legend.position = "right",
    axis.title.y = element_text(colour= "red")
  )

p2