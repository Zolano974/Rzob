#functions related to score results
source("utils/fn_fractures.R")
source("utils/fn_cas.R")
source("utils/theming_plots.R")

#keep only interesting columns
filter_dataset_score_fracture <- function(dataset){
  keep_cols <- c("Oxford","Womac", "HarrisHS", "PMA", "Fracture")
  scores <- dataset[, keep_cols, with = FALSE]
  return(scores)
}

#init empty dataframe for results
init_empty_result_dataset <- function(){
  N <- 1e4
  dataset <- data.frame(
    TestName=character(),
    Moyenne=integer(),
    EcartType=integer(),
    stringsAsFactors=FALSE
  )
  return(dataset)
}

#calculate median and mean for each score for global data
calculate_global_stat_by_score <- function(dataset){
  
  #mean
  mean_oxford = mean(dataset$Oxford)
  mean_womac = mean(dataset$Womac)
  mean_harris = mean(dataset$HarrisHS)
  mean_pma = mean(dataset$PMA)
  
  #median
  sd_oxford = sd(dataset$Oxford)
  sd_womac = sd(dataset$Womac)
  sd_harris = sd(dataset$HarrisHS)
  sd_pma = sd(dataset$PMA)
  
  result = init_empty_result_dataset()
  
  result[1, ] <- list("Oxford",round(mean_oxford, 2), round(sd_oxford, 2))
  result[2, ] <- list("Womac",round(mean_womac, 2), round(sd_womac, 2))
  result[3, ] <- list("HarrisHS",round(mean_harris, 2), round(sd_harris, 2))
  result[4, ] <- list("PMA",round(mean_pma, 2), round(sd_pma, 2))
  
  return(result)
}

generate_score_boxplot <- function(dataset, column){
  
  dataset$FractureX <- as.character(dataset$Fracture)
  dataset$FractureX[dataset$Fracture == "paroi postérieure"] <- "PP"
  dataset$FractureX[dataset$Fracture == "transversale et paroi postérieure"] <- "TR-PP"
  dataset$FractureX[dataset$Fracture == "transversale"] <- "TR"
  dataset$FractureX[dataset$Fracture == "bi colonne"] <- "BC"
  dataset$FractureX[dataset$Fracture == "T"] <- "T"
  dataset$FractureX[dataset$Fracture == "colonne antérieure"] <- "CA"
  
  dataset$FractureX <- factor(
    dataset$FractureX, 
    levels=factor_fracture_x_levels()
  )
  
  plot = dataset %>%
    ggplot(aes_string(x=column, y="FractureX")) + 
    geom_boxplot(
      alpha=0.3,
      color=color_boxplot_scores(column),
      fill = fill_boxplot_scores(column)
    ) +
    coord_flip() +
    default_theming(
      x_title_size = 0,
      x_text_size = 10,
      x_text_angle = 45,
      x_text_vjust = 0.5,
      y_title_angle = 90,
      y_title_size=13,
      y_text_vjust = 0.5
    ) 
  return(plot)
}

#matta/harris - répartition par catégories
generate_pma_harris_by_result <- function(dataset){

  #aggregate Harris
  dataset$HarrisCategory <- as.character(dataset$HarrisHS)
  dataset$HarrisCategory[dataset$HarrisHS < 70] <- "Mauvais"
  dataset$HarrisCategory[dataset$HarrisHS >= 70 & dataset$HarrisHS < 80] <- "Moyen"
  dataset$HarrisCategory[dataset$HarrisHS >= 80 & dataset$HarrisHS < 90] <- "Bon"
  dataset$HarrisCategory[dataset$HarrisHS >= 90] <- "Excellent"
  
  #aggregate PMA
  dataset$PMACategory <- as.character(dataset$PMA)
  dataset$PMACategory[dataset$PMA < 13] <- "Mauvais"
  dataset$PMACategory[dataset$PMA >= 13 & dataset$PMA < 15] <- "Moyen"
  dataset$PMACategory[dataset$PMA >= 15 & dataset$PMA < 18] <- "Bon"
  dataset$PMACategory[dataset$PMA == 18] <- "Excellent"
  
  N <- 1e4
  plotdataset <- data.frame(
    ResultCategory=character(),
    HarrisPercentage=integer(),
    PMAPercentage=integer(),
    stringsAsFactors=FALSE
  )
  
  plotdataset[1, ] <- list(
    "Excellent", 
    #Harris
    percentage_harris_category(dataset, "Excellent"),
    #PMA
    percentage_pma_category(dataset, "Excellent")
  )
  plotdataset[2, ] <- list(
    "Bon", 
    #Harris
    percentage_harris_category(dataset, "Bon"),
    #PMA
    percentage_pma_category(dataset, "Bon")
  )
  plotdataset[3, ] <- list(
    "Moyen", 
    #Harris
    percentage_harris_category(dataset, "Moyen"),
    #PMA
    percentage_pma_category(dataset, "Moyen")
  )
  plotdataset[4, ] <- list(
    "Mauvais", 
    #Harris
    percentage_harris_category(dataset, "Mauvais"),
    #PMA
    percentage_pma_category(dataset, "Mauvais")
  )
  
  
  plotdataset$ResultCategory <- factor(
    plotdataset$ResultCategory,
    levels=c(
      "Excellent",
      "Bon",
      "Moyen",
      "Mauvais"
    )
  )
  
  # plotdataset <- mutate(plotdataset, HarrisPercentage= -HarrisPercentage)
  
  plot = ggplot(plotdataset, aes(x= ResultCategory)) +
    # scale_y_continuous(position="None") +
    geom_col(
      aes(y = PMAPercentage),
      alpha=0.6,
      fill="#ff6600",
      width=0.7
    ) +
    geom_text(aes(y= PMAPercentage, label = paste(PMAPercentage, "%")), vjust = -0.3, colour = "black", size=4) +
    # scale_y_continuous(position="None") +
    geom_col(
      aes(y = -HarrisPercentage),
      alpha=0.6,
      fill="#660055",
      width=0.7
    ) +
    geom_text(aes(y= -HarrisPercentage,label = paste(HarrisPercentage, "%")), vjust = 1.2, colour = "black", size=4)+
    labs (
      title= "",
      y="Harris       -      PMA",
      x="Résultats"
    ) +
    default_theming(
      y_title_angle = 90,
      y_title_vjust = -0.5,
      y_title_size=15,
      x_text_size = 15, 
      x_title_size = 20, 
      title_size = 14,
      legend_title_size = 12,
      y_text_size = 0,
    )
    return(plot)

}

