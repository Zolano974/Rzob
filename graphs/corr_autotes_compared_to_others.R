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

source("utils/fn_correlation_scores_autotest.R")

#filter on useful columns non-empty
mydata = ReccueilR[
  !is.na(Oxford)
  &!is.na(PMA)
  &!is.na(HarrisHS)
  &!is.na(Womac)
  &!is.na(Autotest)
]

#REPRESENTATION OF CORRELATIONS between AUTOTEST and other scores
  
plot_all_corr = generate_histogram_autotest_corr_with_other_scores(mydata)
plot_all_corr
  
plot_linreg_all_scores = generate_4_regression_lines_same_graph(mydata)
plot_linreg_all_scores

# DISPLAY SCATTERING

# Autotest x Oxford
pOxford = generate_scattering_autotest_oxford(mydata)

# Autotest x Womac
pWomac = generate_scattering_autotest_womac(mydata)

# Autotest x Harris
pHarris = generate_scattering_autotest_harris(mydata)

# Autotest x PMA 
pPMA =  generate_scattering_autotest_pma(mydata)

plot_grid(pOxford, pWomac, pHarris, pPMA, labels=c(NA, NA, NA, NA),ncol = 2, nrow = 2)

