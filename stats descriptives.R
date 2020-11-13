#prÃ©-requis: importer le fichier Excel (.xslx) dans R, sous le nom de test1.xlsx
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

#function to format column names
format_column_name <- function(colname) {
  return(tolower(str_replace_all(str_replace_all(colname, " ", "_"), "é", "e")))
}
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

#TODO: ordonner les résultats

# ------------------------------------------------------------ #
# Répartition du type de fracture en fonction du type de trauma
# ------------------------------------------------------------ #
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






