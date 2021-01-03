#ALL
library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(hash)
library(readxl)
library(plyr)

source("utils/fn_cas.R")
source("utils/fn_correlation_scores_autotest.R")
source("utils/fn_count.R")
source("utils/fn_fractures.R")
source("utils/fn_matta.R")
source("utils/fn_scores.R")
source("utils/fn_sf36.R")
source("utils/fn_radios.R")

ReccueilR <- read_excel("ReccueilR.xls")
setDT(ReccueilR)

dataset = ReccueilR[
  !is.na(Matta)
  &!is.na(Arthrose)
  &!is.na(Ossifications)
  &!is.na(OATF)
]

#Arthrose

generate_plot_arthrose_by_matta(dataset)

generate_plot_arthrose_by_fracture(dataset)

#Ossification

generate_plot_ossification_by_matta(dataset)

generate_plot_ossification_by_fracture(dataset)







