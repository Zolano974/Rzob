library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(hash)
library(readxl)
library(RVAideMemoire)

source("utils/fn_cas.R")
source("utils/fn_correlation_scores_autotest.R")
source("utils/fn_count.R")
source("utils/fn_fractures.R")
source("utils/fn_matta.R")
source("utils/fn_scores.R")
source("utils/fn_sf36.R")
source("utils/fn_radios.R")

ReccueilR <- read_excel("./ReccueilR.xls")
savePath = "/home/jhouvenaeghel/Projects/perso/Rzob/results/cleanVI"


setDT(ReccueilR)

# CORRELATION PMA ============================================================================================
#   spearman PMA / Matta     ->  # -> -0.3217049
#   spearman PMA / Age       ->  # -> 0.02442724
#   chi2 PMA / Fracture       -> X-squared = 38.893, df = 35, p-value = 0.2987
#   chi2 PMA / Luxation       -> X-squared = 5, df = 7, p-value = 0.66
# ============================================================================================================


# ---------------------------
# 1 - CORRELATION PMA - MATTA (spearman)
# ---------------------------

#filter data
dataset_pma_matta = ReccueilR[
  !is.na(PMA)
  &!is.na(Matta)
]

#spearman PMA / MATTA
cor(x=dataset_pma_matta$PMA, y=dataset_pma_matta$Matta, method="spearman")  # -> -0.3217049

# -------------------------
# 2 - CORRELATION PMA - Age (spearman)
# -------------------------

#https://www.researchgate.net/figure/Correlation-between-MMSE-total-score-with-age-and-frailty-Spearmans-rank-correlation_fig1_331384106
#Geriatric study calculating correlation between MMSE score and age with Spearman

#filter data
dataset_pma_age = ReccueilR[
  !is.na(PMA)
  &!is.na(Age)
]

#spearman PMA / AGE
cor(x=dataset_pma_age$PMA, y=dataset_pma_age$Age, method="spearman")  # -> 0.02442724

# -------------------------
# 3 - CORRELATION PMA - Type de Fracture (Chi²)
# -------------------------

dataset_pma_fracture = ReccueilR[
  !is.na(PMA)
  &!is.na(Fracture)
]

#chi² PMA / Type Fracture
chisq.test(dataset_pma_fracture$PMA, dataset_pma_fracture$Fracture, correct=FALSE) # -> X-squared = 38.893, df = 35, p-value = 0.2987

# -------------------------
# 4 - CORRELATION PMA - Luxation (Chi²)
# -------------------------

dataset_pma_lux = ReccueilR[
  !is.na(PMA)
  &!is.na(Luxation)
]

#chi² PMA / Luxation
chisq.test(dataset_pma_lux$PMA, dataset_pma_lux$Luxation) # -> X-squared = 5, df = 7, p-value = 0.66


# CORRELATION Oxford ============================================================================================
#   spearman Oxford / Matta     -> 0.3516927 
#   spearman Oxford / Age       -> -0.0884104
#   chi2 Oxford / Fracture      -> X-squared = 93.369, df = 85, p-value = 0.2506
#   chi2 Oxford / Luxation      -> X-squared = 14, df = 17, p-value = 0.6671
# ===============================================================================================================


# ---------------------------
# 1 - CORRELATION Oxford - MATTA (spearman)
# ---------------------------

#filter data
dataset_oxford_matta = ReccueilR[
  !is.na(Oxford)
  &!is.na(Matta)
]

#spearman Oxford / MATTA
cor(x=dataset_oxford_matta$Oxford, y=dataset_oxford_matta$Matta, method="spearman")  # -> 0.3516927

# -------------------------
# 2 - CORRELATION Oxford - Age (spearman)
# -------------------------

#filter data
dataset_oxford_age = ReccueilR[
  !is.na(Oxford)
  &!is.na(Age)
]

#spearman Oxford / AGE
cor(x=dataset_oxford_age$Oxford, y=dataset_oxford_age$Age, method="spearman")  # -> -0.0884104

# -------------------------
# 3 - CORRELATION Oxford - Type de Fracture (Chi²)
# -------------------------

dataset_oxford_fracture = ReccueilR[
  !is.na(Oxford)
  &!is.na(Fracture)
]

#chi² Oxford / Type Fracture
chisq.test(dataset_oxford_fracture$Oxford, dataset_oxford_fracture$Fracture, correct=FALSE) # -> X-squared = 93.369, df = 85, p-value = 0.2506

# -------------------------
# 4 - CORRELATION Oxford - Luxation (Chi²)
# -------------------------

dataset_oxford_lux = ReccueilR[
  !is.na(Oxford)
  &!is.na(Luxation)
]

#chi² Oxford / Type Luxation
chisq.test(dataset_oxford_lux$Oxford, dataset_oxford_lux$Luxation) # ->  X-squared = 14, df = 17, p-value = 0.6671




# CORRELATION HarrisHS ============================================================================================
#   spearman HarrisHS / Matta     ->  # 
#   spearman HarrisHS / Age       ->  # 
#   chi2 HarrisHS / Fracture       -> 
#   chi2 HarrisHS / Luxation       -> 
# ===============================================================================================================


# ---------------------------
# 1 - CORRELATION HarrisHS - MATTA (spearman)
# ---------------------------

#filter data
dataset_harris_matta = ReccueilR[
  !is.na(HarrisHS)
  &!is.na(Matta)
]

#spearman HarrisHS / MATTA
cor(x=dataset_harris_matta$HarrisHS, y=dataset_harris_matta$Matta, method="spearman")  # 

# -------------------------
# 2 - CORRELATION HarrisHS - Age (spearman)
# -------------------------

#filter data
dataset_harris_age = ReccueilR[
  !is.na(HarrisHS)
  &!is.na(Age)
]

#spearman HarrisHS / AGE
cor(x=dataset_harris_age$HarrisHS, y=dataset_harris_age$Age, method="spearman")  # -> 

# -------------------------
# 3 - CORRELATION HarrisHS - Type de Fracture (Chi²)
# -------------------------

dataset_harris_fracture = ReccueilR[
  !is.na(HarrisHS)
  &!is.na(Fracture)
]

#chi² HarrisHS / Type Fracture
chisq.test(dataset_harris_fracture$HarrisHS, dataset_harris_fracture$Fracture, correct=FALSE) #

# -------------------------
# 4 - CORRELATION HarrisHS - Luxation (Chi²)
# -------------------------

dataset_harris_lux = ReccueilR[
  !is.na(HarrisHS)
  &!is.na(Luxation)
]

#chi² HarrisHS / Type Luxation
chisq.test(dataset_harris_lux$HarrisHS, dataset_harris_lux$Luxation) # 



# CORRELATION Womac ============================================================================================
#   spearman Womac / Matta     ->  # 
#   spearman Womac / Age       ->  # 
#   chi2 Womac / Fracture       -> 
#   chi2 Womac / Luxation       -> 
# ===============================================================================================================


# ---------------------------
# 1 - CORRELATION Womac - MATTA (spearman)
# ---------------------------

#filter data
dataset_harris_matta = ReccueilR[
  !is.na(Womac)
  &!is.na(Matta)
]

#spearman Womac / MATTA
cor(x=dataset_harris_matta$Womac, y=dataset_harris_matta$Matta, method="spearman")  # 

# -------------------------
# 2 - CORRELATION Womac - Age (spearman)
# -------------------------

#filter data
dataset_harris_age = ReccueilR[
  !is.na(Womac)
  &!is.na(Age)
]

#spearman Womac / AGE
cor(x=dataset_harris_age$Womac, y=dataset_harris_age$Age, method="spearman")  # -> 

# -------------------------
# 3 - CORRELATION Womac - Type de Fracture (Chi²)
# -------------------------

dataset_harris_fracture = ReccueilR[
  !is.na(Womac)
  &!is.na(Fracture)
]

#chi² Womac / Type Fracture
chisq.test(dataset_harris_fracture$Womac, dataset_harris_fracture$Fracture, correct=FALSE) #

# -------------------------
# 4 - CORRELATION Womac - Luxation (Chi²)
# -------------------------

dataset_harris_lux = ReccueilR[
  !is.na(Womac)
  &!is.na(Luxation)
]

#chi² Womac / Type Luxation
chisq.test(dataset_harris_lux$Womac, dataset_harris_lux$Luxation) # 
