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


setDT(ReccueilR)

dFractureByTrauma = ReccueilR[!is.na(Traumatisme)&!is.na(Fracture)]

p1 = dFractureByTrauma %>%
  ggplot(
    aes(y=Traumatisme, fill=Fracture, order=Fracture)) +
  geom_bar(
    stat='count', 
    orientation="y", 
    alpha=0.9,
    colour="white"
  ) +
  labs (
    title= "Répartition des fractures en fonction du type de trauma",
    x="Nombre de Fractures",
    y="Type de traumatisme"
  ) +
  theme(
    legend.position = "right"
  )

p1
