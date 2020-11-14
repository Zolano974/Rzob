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

#filter on useful columns non-empty
mydata = ReccueilR[
  !is.na(Oxford)
  &!is.na(PMA)
  &!is.na(HarrisHS)
  &!is.na(Womac)
  &!is.na(Autotest)
]


# Autotest x Oxford
pOxford = mydata %>%
  ggplot( aes(x=Autotest, y=Oxford)) +
  geom_point(
    size=2, 
    shape=20,
    color="darkgreen"
  ) +
  geom_smooth(
    color="darkgreen",
    fill="darkgreen",
    aes(x=Autotest, y=Oxford),
    method="lm", 
    se=TRUE, 
    fullrange=FALSE, 
    level=0.995
  )


# Autotest x Womac
pWomac = mydata %>%
  ggplot( aes(x=Autotest, y=Womac)) +
  geom_point(
    size=2, 
    shape=11,
    color="darkblue"
  ) +
  geom_smooth(
    color="darkblue",
    fill="darkblue",
    aes(x=Autotest, y=Womac),
    method="lm", 
    se=TRUE, 
    fullrange=FALSE, 
    level=0.995
  )



# Autotest x Harris
pHarris = mydata %>%
  ggplot( aes(x=Autotest, y=-HarrisHS)) +
  geom_point(
    size=2, 
    shape=17,
    color="darkred"
  ) +
  geom_smooth(
    color="darkred",
    fill="darkred",
    aes(x=Autotest, y=-HarrisHS),
    method="lm", 
    se=TRUE, 
    fullrange=FALSE, 
    level=0.995
  )
p3

pPMA = 
  mydata %>%
  ggplot( aes(x=Autotest, y=-PMA)) +
  geom_point(
    size=2, 
    shape=14,
    color="orange"
  ) +
  geom_smooth(
    color="darkorange",
    fill="darkorange",
    aes(x=Autotest, y=-PMA),
    method="lm", 
    se=TRUE, 
    fullrange=FALSE, 
    level=0.995
  )
pPMA


plot_grid(pOxford, pWomac, pHarris, pPMA, labels=c("Oxford", "Womac", "Harris", "PMA"),ncol = 2, nrow = 2)
