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
p1 = mydata %>%
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
p2 = mydata %>%
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
p3 = mydata %>%
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

p4 = 
  mydata %>%
  ggplot( aes(x=Autotest, y=-PMA)) +
  geom_point(
    size=2, 
    shape=17,
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
p4


plot_grid(p1, p2, labels=c("ZOBY", "ZOBA"), ncol = 2, nrow = 1)

plot_grid(p1, p2, p3, p4, labels=c("Oxford", "Womac", "Harris", "PMA"),ncol = 2, nrow = 2)
