#ALL

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
    p2bis = generate_plot_patients_repartition_by_voie_abord(generaldata)
    p2bis
    
    # -> répartition des cas par XP chirurgien
    p3 = generate_plot_patients_repartition_by_xp_chir(generaldata)
    p3
    
    # -> répartition des cas par score MATTA
    p4 = generate_plot_patients_repartition_by_matta(generaldata)
    p4

# II - Repartition Fractures
    
    # ----------------------------------------- #
    # I -Fracture en fonction du type de trauma #
    # ----------------------------------------- #
    
    #filter the initial data
    fracturedata1 = ReccueilR[!is.na(Traumatisme)&!is.na(Fracture)]
    
    #generate plot
    fractureEnFonctionDuTrauma = generate_plot_fracture_by_traumatisme(fracturedata)
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
    # View(scoresdata)
    
    #STATS GLOBALE
    global_stats_table = calculate_global_stat_by_score(scoresdata)
    # View(global_stats_table)
    
    #SOUS GROUPES PAR TYPE DE FRACTURE
    
    #APPROCHE I -> 1 tableau pour chaque type de fracture
    scores_t_data <- scoresdata[Fracture == "T"]
    T_stats_table = calculate_global_stat_by_score(scores_t_data)
    T_stats_table
    
    bicolonne_data <- scoresdata[Fracture == "bi colonne"]
    bicolonne_stats_table = calculate_global_stat_by_score(bicolonne_data)
    bicolonne_stats_table
    
    colonneanterieure_data <- scoresdata[Fracture == "colonne antérieure"]
    colonneanterieure_stats_table = calculate_global_stat_by_score(colonneanterieure_data)
    colonneanterieure_stats_table
    
    paroiposterieure_data <- scoresdata[Fracture == "paroi postérieure"]
    paroiposterieure_stats_table = calculate_global_stat_by_score(paroiposterieure_data)
    paroiposterieure_stats_table
    
    transversale_data <- scoresdata[Fracture == "transversale"]
    transversale_stats_table = calculate_global_stat_by_score(transversale_data)
    transversale_stats_table
    
    transversale_paroipost_data <- scoresdata[Fracture == "transversale et paroi postérieure"]
    transversale_paroipost_stats_table = calculate_global_stat_by_score(transversale_paroipost_data)
    transversale_paroipost_stats_table
    
    #Approche II -> un seul tableau
    
    mean_by_score <- apply(scoresdata[,1:4],2,tapply, scoresdata$Fracture, mean)
    # View(mean_by_score)
    
    median_by_score <- apply(scoresdata[,1:4],2,tapply, scoresdata$Fracture, median)
    # View(median_by_score)
    
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
    
    # Autotest x Oxford
    pOxford = generate_scattering_autotest_oxford(mydata)
    
    # Autotest x Womac
    pWomac = generate_scattering_autotest_womac(mydata)
    
    # Autotest x Harris
    pHarris = generate_scattering_autotest_harris(mydata)
    
    # Autotest x PMA 
    pPMA =  generate_scattering_autotest_pma(mydata)
    
    plot_grid(pOxford, pWomac, pHarris, pPMA, labels=c(NA, NA, NA, NA),ncol = 2, nrow = 2)
    
    
    