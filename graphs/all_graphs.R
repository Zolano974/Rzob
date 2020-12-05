#ALL
library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)

source("utils/fn_fractures.R")
source("utils/fn_scores.R")
source("utils/fn_cas.R")
source("utils/fn_matta.R")

setDT(ReccueilR)

# I - Global repartition des cas

    # -> répartition des cas par type de trauma
    generaldata = ReccueilR[
      !is.na(Oxford)
      &!is.na(PMA)
      &!is.na(HarrisHS)
      &!is.na(Womac)
      &!is.na(Fracture)
    ]
    
    p1 = generate_plot_patients_repartition_by_traumatisme(generaldata)
    p1
    
    # -> répartition des cas par type de fracture
    
    p2 = generate_plot_patients_repartition_by_fracture(generaldata)
    p2
    
    # -> repartition des cas par voie d'abord 
    p3 = generate_plot_patients_repartition_by_voie_abord(generaldata)
    p3
    
    # -> répartition des cas par XP chirurgien
    p4 = generate_plot_patients_repartition_by_xp_chir(generaldata)
    p4
    
    # -> répartition des cas par score MATTA
    p5 = generate_plot_patients_repartition_by_matta(generaldata)
    p5

# II - Repartition Fractures
    
    # ----------------------------------------- #
    # I -Fracture en fonction du type de trauma #
    # ----------------------------------------- #
    
    #filter the initial data
    fracturedata1 = ReccueilR[!is.na(Traumatisme)&!is.na(Fracture)]
    
    #generate plot
    fractureEnFonctionDuTrauma = generate_plot_fracture_by_traumatisme(fracturedata1)
    fractureEnFonctionDuTrauma
    
    # ----------------------------------------- #
    # I -Fracture en fonction de l'age          #
    # ----------------------------------------- #
    
    fracturedata2 = ReccueilR[!is.na(Age)&!is.na(Fracture)]
    
    fractureEnFonctionDeLage = generate_plot_fracture_by_age(fracturedata2)
    fractureEnFonctionDeLage
    
    # ----------------------------------------- #
    # I -Fracture en fonction de l'xp chir      #
    # ----------------------------------------- #
    
    fracturedata3 = ReccueilR[!is.na(Chirurgien)&!is.na(Fracture)]
    fracture_x_chir = generate_histogram_fracture_expchir(fracturedata3)
    fracture_x_chir
    
# III - Matta
    
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
    
    matta_x_chir = generate_histogram_mata_expchir(mattadata)
    matta_x_chir
    # p0
    
    # --------------------------------------------------  #
    # II - Score de Matta en fonction du type de fracture #
    # --------------------------------------------------  #
    
    matta_x_fracture_global = generate_histogram_mata_fracture(mattadata)
    matta_x_fracture_global

# IV - Scores    

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
    View(scoresdata)
    
    #STATS GLOBALE
    global_stats_table = calculate_global_stat_by_score(scoresdata)
    global_stats_table
    # View(global_stats_table)
    
    #SOUS GROUPES PAR TYPE DE FRACTURE
    
    #Approche II -> un seul tableau
    
    # mean_by_score <- apply(scoresdata[,1:4],2,tapply, scoresdata$Fracture, mean)
    # mean_by_score
    
    sd_by_score <- apply(scoresdata[,1:4],2,tapply, scoresdata$Fracture, sd)
    sd_by_score
    
    #Boxplots
    
    oxford_bp = generate_score_boxplot(scoresdata, "Oxford")
    
    womac_bp = generate_score_boxplot(scoresdata, "Womac")
    harris_bp = generate_score_boxplot(scoresdata, "HarrisHS")
    pma_bp = generate_score_boxplot(scoresdata, "PMA")
    
    plot_grid(oxford_bp, womac_bp, labels=c(NA, NA),ncol = 2, nrow = 1)
    plot_grid(harris_bp, pma_bp, labels=c(NA, NA),ncol = 2, nrow = 1)
    
    
# V - SF36 (TODO)
    
# VI -Radios (TODO, TOBEDEFINED)
    
# VII - Autotest
    
    
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

    
    
    