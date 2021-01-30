#ALL
library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(hash)
library(readxl)

source("utils/fn_cas.R")
source("utils/fn_correlation_scores_autotest.R")
source("utils/fn_count.R")
source("utils/fn_fractures.R")
source("utils/fn_matta.R")
source("utils/fn_scores.R")
source("utils/fn_sf36.R")
source("utils/fn_radios.R")

ReccueilR <- read_excel("../ReccueilR.xls")

savePath = "/home/zolano/Documents/TheseSophie/R/results/clean5"


setDT(ReccueilR)

# ============================== #
# I - Global repartition des cas #
# ============================== #

    # -> répartition des cas par type de trauma
    generaldata = ReccueilR
    
    # -> réprtition des cas par type de trauma
    generate_plot_patients_repartition_by_traumatisme(generaldata)
    
    # -> répartition des cas par type de fracture
    generate_plot_patients_repartition_by_fracture(generaldata)
    
    # -> repartition des cas par voie d'abord       
    generate_plot_patients_repartition_by_voie_abord(generaldata)
    
    # -> répartition des cas par XP chirurgien
    generate_plot_patients_repartition_by_xp_chir(generaldata)
    
    # -> répartition des cas par score MATTA
    generate_plot_patients_repartition_by_matta(generaldata)
  
  # ========================== #
  # II - Repartition Fractures
  # ========================== #

    # ----------------------------------------- #
    # I -Fracture en fonction du type de trauma #
    # ----------------------------------------- #
    
    #filter the initial data
    fracturedata1 = ReccueilR[!is.na(Traumatisme)&!is.na(Fracture)]
    
    #generate plot
    generate_plot_fracture_by_traumatisme(fracturedata1)
    
    # ----------------------------------------- #
    # I -Fracture en fonction de l'age          #
    # ----------------------------------------- #
    
    fracturedata2 = ReccueilR[!is.na(Age)&!is.na(Fracture)]
    generate_plot_fracture_by_age(fracturedata2)
    
    # ----------------------------------------- #
    # I -Fracture en fonction de l'xp chir      #
    # ----------------------------------------- #
    
    fracturedata3 = ReccueilR[!is.na(Chirurgien)&!is.na(Fracture)]
    generate_histogram_fracture_expchir(fracturedata3)
    
# =========== #    
# III - Matta #
# =========== #    
    
    mattadata = ReccueilR[
      !is.na(Matta)
      &!is.na(Chirurgien)
    ]
    
    chirJunior = ReccueilR[Chirurgien == "Junior"]
    chirSenior = ReccueilR[Chirurgien == "Senior"]
    chirExpermente = ReccueilR[Chirurgien == "Expérimenté"]
    
    # --------------------------------------------------  #
    # I - Score de Matta en fonction de l'exp chirurgien  #
    # --------------------------------------------------  #
    generate_histogram_mata_expchir(mattadata)
    
    # p0
    
    # --------------------------------------------------  #
    # II - Score de Matta en fonction du type de fracture #
    # --------------------------------------------------  #
    generate_histogram_mata_fracture(mattadata)

# ============= #
# IV - Scores   #
# ============= #
    
    #filter the initial data
    scoredata = ReccueilR[
      !is.na(Oxford)
      &!is.na(PMA)
      &!is.na(HarrisHS)
      &!is.na(Womac)
      &!is.na(Fracture)
    ]
    
    
    #keep only 4 scores + fracture type
    scoresdata = filter_dataset_score_fracture(scoredata)
    # View(scoresdata)
    
    #STATS GLOBALE
    global_stats_table = calculate_global_stat_by_score(scoresdata)
    global_stats_table
    write.csv(global_stats_table, "/home/zolano/Documents/TheseSophie/R/zob.csv", row.names = TRUE)
    # View(global_stats_table)
    
    #SOUS GROUPES PAR TYPE DE FRACTURE
    
    #Approche II -> un seul tableau
    
    mean_by_score <- apply(scoresdata[,1:4],2,tapply, scoresdata$Fracture, mean)
    mean_by_score
    
    sd_by_score <- apply(scoresdata[,1:4],2,tapply, scoresdata$Fracture, sd)
    sd_by_score
    
    #Boxplots
    
    oxford_bp = generate_score_boxplot(scoresdata, "Oxford")
    
    womac_bp = generate_score_boxplot(scoresdata, "Womac")
    harris_bp = generate_score_boxplot(scoresdata, "HarrisHS")
    pma_bp = generate_score_boxplot(scoresdata, "PMA")
    
    plot_grid(oxford_bp, womac_bp, labels=c(NA, NA),ncol = 2, nrow = 1)
    plot_grid(harris_bp, pma_bp, labels=c(NA, NA),ncol = 2, nrow = 1)
    
# ======== #    
# V - SF36 #
# ======== #

  setDT(sf36)
  
    # ------------------------------- #
    # Moyennes des notes SF36 globale #
    # ------------------------------- #
    
    calculate_sf36_means(sf36)
    generate_SF36_circularo_barplot(sf36)
    
    # ------------------------------------------- #
    # Moyenne des notes SF36 par type de fracture #
    # ------------------------------------------- #
    
    #paroi post
    calculate_sf36_means(sf36[typefracture == "paroi post"])
    generate_SF36_circularo_barplot(sf36[typefracture == "paroi post"])
    
    #transversale
    calculate_sf36_means(sf36[typefracture == "transversale"])
    generate_SF36_circularo_barplot(sf36[typefracture == "transversale"])
    
    #transversale et paroipost
    calculate_sf36_means(sf36[typefracture == "transversale et paroi post"])
    generate_SF36_circularo_barplot(sf36[typefracture == "transversale et paroi post"])
    
    #colonne antérieure
    calculate_sf36_means(sf36[typefracture == "colonne antérieure"])
    generate_SF36_circularo_barplot(sf36[typefracture == "colonne antérieure"])
    
    #bi colonne
    calculate_sf36_means(sf36[typefracture == "bi colonne"])
    generate_SF36_circularo_barplot(sf36[typefracture == "bi colonne"])
    
    #T
    calculate_sf36_means(sf36[typefracture == "T"])
    generate_SF36_circularo_barplot(sf36[typefracture == "T"])
    
# ============================== #       
# VI -Radios (TODO, TOBEDEFINED)
# ============================== #       
    
    
    radios_data = ReccueilR[
      !is.na(Matta)
      &!is.na(Arthrose)
      &!is.na(Ossifications)
    ]
    
    #Arthrose
    
    generate_plot_arthrose_by_matta(radios_data)
    
    generate_plot_arthrose_by_fracture(radios_data)
    
    #Ossification
    
    generate_plot_ossification_by_matta(radios_data)
    
    generate_plot_ossification_by_fracture(radios_data)
    ggsave(
      filename="21_RADIOS_OSSIFICATION_BY_FRACTURE.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
# ============== #   
# VII - Autotest
# ============== #   
    
    autotestdata = ReccueilR[
      !is.na(Oxford)
      &!is.na(PMA)
      &!is.na(HarrisHS)
      &!is.na(Womac)
      &!is.na(Autotest)
    ]
    
    # Correlation autotest to other scores
    plot_all_corr = generate_histogram_autotest_corr_with_other_scores(autotestdata)
    plot_all_corr
    
    #linear regression autotest vs other scores
    grid_linear_reg = generator_lineareg_plot_grid_autotest_vs_others(autotestdata)
    grid_linear_reg

    
    
    