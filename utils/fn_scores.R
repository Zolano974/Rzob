#functions related to score results
source("utils/theming_plots.R")

#keep only interesting columns
filter_dataset_score_fracture <- function(dataset){
  keep_cols <- c("Oxford","Womac", "HarrisHS", "PMA", "Fracture")
  scores <- fulldata[, keep_cols, with = FALSE]
  return(scores)
}

#init empty dataframe for results
init_empty_result_dataset <- function(){
  N <- 1e4
  dataset <- data.frame(
    TestName=character(),
    Mean=integer(),
    Median=integer(),
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
  median_oxford = median(dataset$Oxford)
  median_womac = median(dataset$Womac)
  median_harris = median(dataset$HarrisHS)
  median_pma = median(dataset$PMA)
  
  result = init_empty_result_dataset()
  
  result[1, ] <- list("Oxford",round(mean_oxford, 2), round(median_oxford, 2))
  result[2, ] <- list("Womac",round(mean_womac, 2), round(median_womac, 2))
  result[3, ] <- list("HarrisHS",round(mean_harris, 2), round(median_harris, 2))
  result[4, ] <- list("PMA",round(mean_pma, 2), round(median_pma, 2))
  
  return(result)
}

generate_score_boxplot <- function(dataset, column){
  plot = dataset %>%
    ggplot(aes_string(x=column, y="Fracture")) + 
    geom_boxplot(
      alpha=0.3,
      color=color_boxplot_scores(column),
      fill = fill_boxplot_scores(column)
    ) +
    coord_flip() +
    stat_summary(fun.y=mean, geom="point", shape=20, size=4) +
    # scale_fill_brewer(palette="BuPu")
    default_theming(
      x_title_size = 10,
      x_text_size = 5,
      x_text_angle = 45,
      x_text_vjust = 0.5,
      y_title_angle = 90,
      y_title_size=13
    )
    # theme(
    #   axis.text.x = element_text(angle=45, vjust=0.5)
    # )
  return(plot)
}

