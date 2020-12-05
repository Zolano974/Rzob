#functions related to score results
source("utils/fn_fractures.R")
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
    stat_summary(fun.y=mean, geom="point", shape=20, size=4) +
    # scale_fill_brewer(palette="BuPu")
    default_theming(
      x_title_size = 0,
      x_text_size = 10,
      x_text_angle = 45,
      x_text_vjust = 0.5,
      y_title_angle = 90,
      y_title_size=13,
      y_text_vjust = 0.5
    ) 
    # labs (
    #   x="Fracture",
    # ) 
    # theme(
      # axis.text.x = element_text(angle=45, vjust=0.5)
    # )
  return(plot)
}

