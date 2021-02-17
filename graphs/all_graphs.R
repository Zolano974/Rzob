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

ReccueilR <- read_excel("./ReccueilR.xls")


# savePath = "/home/zolano/Documents/TheseSophie/R/results/clean5"
savePath = "/home/jhouvenaeghel/Projects/perso/Rzob/results/cleanV"


setDT(ReccueilR)

# ============================== #
# I - Global repartition des cas #
# ============================== #

    # -> répartition des cas par type de trauma
    generaldata = ReccueilR
    
    # -> réprtition des cas par type de trauma
    generate_plot_patients_repartition_by_traumatisme(generaldata)
    ggsave(
      filename="1_GENERAL_repartition_par_trauma.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
    # -> répartition des cas par type de fracture
    generate_plot_patients_repartition_by_fracture(generaldata)
    ggsave(
      filename="2_GENERAL_repartition_par_type_fracture.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
    # -> repartition des cas par voie d'abord       
    generate_plot_patients_repartition_by_voie_abord(generaldata)
    ggsave(
      filename="3_GENERAL_repartition_par_voiedabord.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
    # -> répartition des cas par XP chirurgien
    generate_plot_patients_repartition_by_xp_chir(generaldata)
    ggsave(
      filename="4_GENERAL_repartition_par_xp_chir.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
    # -> répartition des cas par score MATTA
    generate_plot_patients_repartition_by_matta(generaldata)
    ggsave(
      filename="5_GENERAL_repartition_par_matta.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
  
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
    ggsave(
      filename="6_FRACTURE_repartition_par_trauma.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
    # ----------------------------------------- #
    # I -Fracture en fonction de l'age          #
    # ----------------------------------------- #
    
    fracturedata2 = ReccueilR[!is.na(Age)&!is.na(Fracture)]
    generate_plot_fracture_by_age(fracturedata2)
    ggsave(
      filename="7_FRACTURE_repartition_par_age.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
    # ----------------------------------------- #
    # I -Fracture en fonction de l'xp chir      #
    # ----------------------------------------- #
    
    fracturedata3 = ReccueilR[!is.na(Chirurgien)&!is.na(Fracture)]
    generate_histogram_fracture_expchir(fracturedata3)
    ggsave(
      filename="8_FRACTURE_repartition_par_xp_chir.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )    
    
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
    ggsave(
      filename="9_MATTA_repartition_par_xp_chir.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )    
    
    # p0
    
    # --------------------------------------------------  #
    # II - Score de Matta en fonction du type de fracture #
    # --------------------------------------------------  #
    generate_histogram_mata_fracture(mattadata)
    ggsave(
      filename="10_MATTA_repartition_par_fracture.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )        

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
    write.csv(global_stats_table, "/home/jhouvenaeghel/Projects/perso/Rzob/results/cleanV/11_SCORES_mean_sd_global.csv", row.names = TRUE)
    # View(global_stats_table)
    
    #SOUS GROUPES PAR TYPE DE FRACTURE
    
    #Approche II -> un seul tableau
    
    mean_by_score <- apply(scoresdata[,1:4],2,tapply, scoresdata$Fracture, mean)
    mean_by_score
    write.csv(mean_by_score, "/home/jhouvenaeghel/Projects/perso/Rzob/results/cleanV/12_SCORES_mean_sd_par_type_fracture_moyenne.csv", row.names = TRUE)
    
    sd_by_score <- apply(scoresdata[,1:4],2,tapply, scoresdata$Fracture, sd)
    sd_by_score
    write.csv(sd_by_score, "/home/jhouvenaeghel/Projects/perso/Rzob/results/cleanV/12_SCORES_mean_sd_par_type_fracture_ecartype.csv", row.names = TRUE)
    
    #Boxplots
    
    oxford_bp = generate_score_boxplot(scoresdata, "Oxford")
    womac_bp = generate_score_boxplot(scoresdata, "Womac")
    harris_bp = generate_score_boxplot(scoresdata, "HarrisHS")
    pma_bp = generate_score_boxplot(scoresdata, "PMA")
    
    
    plot_grid(oxford_bp, womac_bp, labels=c(NA, NA),ncol = 2, nrow = 1)
    ggsave(
      filename="13_SCORES_boxplot_ligne1.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 12.5,
      units = "cm"
    )       
    
    plot_grid(harris_bp, pma_bp, labels=c(NA, NA),ncol = 2, nrow = 1)
    ggsave(
      filename="14_SCORES_boxplot_ligne2.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 12.5,
      units = "cm"
    )        
    
# ======== #    
# V - SF36 #
# ======== #
  sf36 <- read_excel("./sf36.xlsx")
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
    #22_SF36_moyennes_global.png
    
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
    ggsave(
      filename="18_RADIOS_ARTHROSE_BY_MATTA.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
    generate_plot_arthrose_by_fracture(radios_data)
    ggsave(
      filename="19_RADIOS_ARTHROSE_BY_FRACTURE.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
    #Ossification
    
    generate_plot_ossification_by_matta(radios_data)
    ggsave(
      filename="20_RADIOS_OSSIFICATION_BY_MATTA.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
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
    generate_histogram_autotest_corr_with_other_scores(autotestdata)
    ggsave(
      filename="15_SCORES_correlations_autotest.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 25,
      height = 15,
      units = "cm"
    )
    
    #linear regression autotest vs other scores
    generator_lineareg_plot_grid_autotest_vs_others(autotestdata)
    ggsave(
      filename="16_SCORES_linear_reg_autotest.png", 
      plot=last_plot(),
      device = png(),
      path =  savePath,
      width = 30,
      height = 25,
      units = "cm"
    )
    grid_linear_reg

    
    
    