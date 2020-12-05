# TODO

#répartition globale 

#repartition globale [DONE]

 # [OK] -> virer l'échelle a gauche
 # [OK] -> afficher en % le comptage et l'échelle (ajout de colonne dynamique)

#scores [DONE] 

  # [OK] -> ecar-type dans les tableaux, pas mediane

#autotest 

  # [OK] ->  graph 4 corr sperarman : appliquer code couleur

  # [OK] -> graph 4 regressions lineaires des scores : revoir legende (virer autotest) et theme

#SF36

# -> moeyenne globale de chaque partie

# -> moyenne globale de chaque partie par type de fracture




source("utils/fn_fractures.R")
source("utils/fn_scores.R")
# -> répartition des cas par type de trauma
setDT(ReccueilR)
generaldata = ReccueilR[
  !is.na(Oxford)
  &!is.na(PMA)
  &!is.na(HarrisHS)
  &!is.na(Womac)
  &!is.na(Fracture)
]


N <- 1e4
plotdataset <- data.frame(
  Matta=character(),
  Percentage=integer(),
  stringsAsFactors=FALSE
)

plotdataset[1, ] <- list("Anatomique", percentage_matta(generaldata, "1"))
plotdataset[2, ] <- list("Satisfaisant", percentage_matta(generaldata, "2"))
plotdataset[3, ] <- list("Non Satisfaisant", percentage_matta(generaldata, "3"))



#defining order for "Chirurgien"  (/!\ if orientation=x order needs to be reversed)
plotdataset$Matta <- factor(
  plotdataset$Matta, 
  levels=c(
    "Anatomique", 
    "Satisfaisant",
    "Non Satisfaisant"
  )
)

plotdataset %>%
  ggplot( aes(x = Matta, y = Percentage)) +
  geom_col(
    alpha=0.6,
    fill=color_generic_stats(),
    width=0.7
  ) +
  geom_text(aes(label = paste(Percentage, "%")), vjust = -0.3, colour = "black") +
  labs (
    title= "Répartition des patients en fonction de l'expérience du chirurgien",
    y="Nombre de cas",
    x="Expérience du Chirurgien"
  ) +
  default_theming(
    y_title_angle = 90,
    title_size = 14,
    legend_title_size = 12,
    y_text_size = 0,
    y_title_size = 0
  )


library(gcookbook) # Load gcookbook for the cabbage_exp data set

# View(cabbage_exp)
# Below the top
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
  geom_col() +
  geom_text(aes(label = Weight), vjust = 1.5, colour = "white")

# Above the top
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
  geom_col() +
  geom_text(aes(label = Weight), vjust = -0.2)


dataset$FractureX <- as.character(dataset$Fracture)
dataset$FractureX[dataset$Fracture == "paroi postérieure"] <- "PP"
dataset$FractureX[dataset$Fracture == "transversale et paroi postérieure"] <- "TR-PP"
dataset$FractureX[dataset$Fracture == "transversale"] <- "TR"
dataset$FractureX[dataset$Fracture == "bi colonne"] <- "BC"
dataset$FractureX[dataset$Fracture == "T"] <- "T"
dataset$FractureX[dataset$Fracture == "colonne antérieure"] <- "CA"

dataset$FractureX <- factor(
  dataset$FractureX, 
  levels=c(
    "PP",
    "TR-PP",
    "TR",
    "BC",
    "T",
    "CA"
  )
)

plot = dataset %>%
  ggplot(aes(x=FractureX)) +
  geom_bar(
    stat='count', 
    orientation="x", 
    alpha=0.9,
    colour="white",
    fill=color_generic_stats(),
    width=0.7
  ) +
  labs (
    title= "Répartition des patients en fonction du type de fracture",
    y="Nombre de cas",
    x="Type de Fracture"
  ) +
  default_theming(
    y_title_angle = 90,
    title_size = 14,
    legend_title_size = 12,
    x_text_angle = 45,
    x_text_size = 10,
    x_text_vjust = 0.5
  )

return(plot)


