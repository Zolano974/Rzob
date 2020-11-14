#Type de Fracture en fonction du type de Traumatisme

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

#SPEARMAN CORRELATION CALCULATION

  # calcul de la correlation de spearman entre Autotest (x) et Oxford (y)
  
  cor.test(formula=~Oxford+Autotest,data=mydata,method = "spearman",alternative = "greater")# -> (rho=0.8547031, p=1e-07)
  corOxford = cor(x=mydata$Autotest, y=mydata$Oxford, method="spearman") # -> 0.8547031
  
  # calcul de la correlation de spearman entre Autotest (x) et Womac (y)
  cor.test(formula=~Womac+Autotest,data=mydata,method = "spearman",alternative = "greater")# -> (rho=0.931137, p=5e-011)
  corWomac = cor(x=mydata$Autotest, y=mydata$Womac, method="spearman") # -> 0.931137
  
  # calcul de la correlation de spearman entre Autotest (x) et Harris Hip Score (y)
  cor.test(formula=~HarrisHS+Autotest,data=mydata,method = "spearman",alternative = "less")# -> (rho=-0.866056, p=4e-08)
  corHarris = cor(x=mydata$Autotest, y=mydata$HarrisHS, method="spearman") # -> -0.866056
  
  # calcul de la correlation de spearman entre Autotest (x) et PMA  (y)
  
  cor.test(formula=~PMA+Autotest,data=mydata,method = "spearman", alternative="less")# -> (rho=-0.8928448, p=1e-08)
  corPMA = cor(x=mydata$Autotest, y=mydata$PMA, method="spearman") # -> 0.8928448

#REPRESENTATION OF CORRELATIONS
  
  #preparing a dataframe to store the result of previous stats calculations 
  N <- 1e4
  testcors <- data.frame(
                  TestName=character(),
                  Rho=integer(),
                  stringsAsFactors=FALSE
                ) 
  
  #storing results in dataframe
  testcors[1, ] <- list("Oxford",abs(corOxford))
  testcors[2, ] <- list("Womac", abs(corWomac))
  testcors[3, ] <- list("Harris", abs(corHarris))
  testcors[4, ] <- list("PMA", abs(corPMA))
  
  #plotting dataframe as histogram
  p0 = testcors %>%
    ggplot(aes(x=TestName, y=Rho, label=round(Rho, 3))) +
    geom_bar(
      aes(fill = Rho),
      stat='identity',
      orientation="x",
      colour="white"
    ) +
    geom_text(size = 4, position = position_stack(vjust = 0.9), color="white") +
    labs (
      title= "Corrélation de l'auto-test aux autres tests",
      x="Tests Connus",
      y="Corrélation de Spearman"
    ) +
    theme(
      legend.position = "right",
      axis.title.y = element_text(colour= "red")
    )
p0
  
# Display 4 linear regression lines on 1 plot
p0bis = mydata %>%
  ggplot( aes(x=Autotest, y=Oxford)) +
  geom_smooth(
    color="darkgreen",
    fill="darkgreen",
    aes(x=Autotest, y=Oxford),
    method="lm", 
    se=FALSE, 
    fullrange=FALSE, 
    level=0.95
  ) +   
  geom_smooth(
    color="darkblue",
    fill="darkblue",
    aes(x=Autotest, y=Womac),
    method="lm", 
    se=FALSE, 
    fullrange=FALSE, 
    level=0.95
  ) +
  geom_smooth(
    color="darkred",
    fill="darkred",
    aes(x=Autotest, y=(-HarrisHS)),
    method="lm", 
    se=FALSE, 
    fullrange=FALSE, 
    level=0.95
  ) +
  geom_smooth(
    color="darkorange",
    fill="darkorange",
    aes(x=Autotest, y=(-PMA)),
    method="lm", 
    se=FALSE, 
    fullrange=FALSE, 
    level=0.95
  )
p0bis

# DISPLAY SCATTERING


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

