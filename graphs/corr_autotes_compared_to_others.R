#Type de Fracture en fonction du type de Traumatisme

#prerequisite: importer le fichier Excel (.xslx) dans R, sous le nom de test1.xlsx
install.packages("data.table")
install.packages("stringr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")
#on charge les donnÃ©es 
library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)

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
                  CorrWithAutoTest=integer(),
                  stringsAsFactors=FALSE
                ) 
  
  #storing results in dataframe
  testcors[1, ] <- list("Oxford",abs(corOxford * 100))
  testcors[2, ] <- list("Womac", abs(corWomac*100))
  testcors[3, ] <- list("Harris", abs(corHarris*100))
  testcors[4, ] <- list("PMA", abs(corPMA * 100))
  
  #plotting dataframe as histogram
  p0 = testcors %>%
    ggplot(aes(x=TestName, y=CorrWithAutoTest)) +
    geom_bar(
      aes(fill = CorrWithAutoTest),
      stat='identity',
      orientation="x",
      colour="white"
    ) +
    labs (
      title= "Corrélation de l'auto-test aux autres tests",
      x="Tests Connus",
      y="Corrélation de Spearman"
    ) +
    theme(
      legend.position = "right",
      axis.title.y = element_text(colour= "red")
    )
  
# p0

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
    label="Oxford",
    aes(x=Autotest, y=Oxford),
    method="auto", 
    se=TRUE, 
    fullrange=FALSE, 
    level=0.95
  )
# p1

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
    aes(x=Autotest, y=Womac),
    method="lm", 
    se=TRUE, 
    fullrange=FALSE, 
    level=0.95
  )
# p2



