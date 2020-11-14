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

p1 = dFractureByAge %>%
  ggplot(aes(x=Age, fill=factor(
        Fracture, 
        #here we define the order of the stacked values
        levels=c(
          "T",
          "colonne antérieure",
          "transversale",
          "paroi postérieure", 
          "transversale et paroi postérieure",
          "bi colonne"
        )
      )
    )
  ) +
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

p1

# p2 = dFractureByAge %>%
#   ggplot(aes(x=Age, fill=Fracture, order=Fracture)) +
#   geom_histogram(
#     binwidth=15,
#     orientation="x",
#     colour="white"
#   ) +
#   labs (
#     title= "Répartition des fractures en fonction de l'âge",
#     x="Classe d'âge",
#     y="Nombre de fractures"
#   ) +
#   theme(
#     legend.position = "right",
#     axis.title.y = element_text(colour= "red")
#   )
# 
# p2