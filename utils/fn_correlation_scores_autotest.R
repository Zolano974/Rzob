#functions for correlations between autotest and other tests

source("utils/theming_plots.R")
source("utils/theming_colors.R")

#scattering for Oxford x Autotest
generate_scattering_autotest_oxford <- function(dataset){
  
  plot = dataset %>%
    ggplot( aes(x=Autotest, y=Oxford)) +
    geom_point(
      size=2, 
      shape=20,
      color=color_boxplot_scores("Oxford"),
    ) +
    geom_smooth(
      color=color_boxplot_scores("Oxford"),
      fill=color_boxplot_scores("Oxford"),
      aes(x=Autotest, y=Oxford),
      method="lm", 
      se=TRUE, 
      fullrange=FALSE, 
      level=0.995
    ) + 
    labs (
      title= "",
      y="",
      x="Oxford"
    ) +
    default_theming()
  
  return(plot)
}

#scattering for Womac x Autotest
generate_scattering_autotest_womac <- function(dataset){
  plot = dataset %>%
    ggplot( aes(x=Autotest, y=Womac)) +
    geom_point(
      size=2, 
      shape=22,
      color=color_boxplot_scores("Womac")
    ) +
    geom_smooth(
      color=color_boxplot_scores("Womac"),
      fill=color_boxplot_scores("Womac"),
      aes(x=Autotest, y=Womac),
      method="lm", 
      se=TRUE, 
      fullrange=FALSE, 
      level=0.995
    ) + 
    labs (
      title= "",
      y="",
      x="Womac"
    ) +
    default_theming()
  return(plot)
} 

#scattering for Harris HS x Autotest
generate_scattering_autotest_harris <- function(dataset){
  plot = dataset %>%
    ggplot( aes(x=Autotest, y=-HarrisHS)) +
    geom_point(
      size=2, 
      shape=17,
      color=color_boxplot_scores("HarrisHS")
    ) +
    geom_smooth(
      color=color_boxplot_scores("HarrisHS"),
      fill=color_boxplot_scores("HarrisHS"),
      aes(x=Autotest, y=-HarrisHS),
      method="lm", 
      se=TRUE, 
      fullrange=FALSE, 
      level=0.995
    ) + 
    labs (
      title= "",
      y="",
      x="Harris HS"
    ) +
    default_theming()
  return(plot)
} 

#scattering for PMA x Autotest
generate_scattering_autotest_pma <- function(dataset){
  plot = dataset %>%
    ggplot( aes(x=Autotest, y=-PMA)) +
    geom_point(
      size=2, 
      shape=17,
      color=color_boxplot_scores("PMA")
    ) +
    geom_smooth(
      color=color_boxplot_scores("PMA"),
      fill=color_boxplot_scores("PMA"),
      aes(x=Autotest, y=-PMA),
      method="lm", 
      se=TRUE, 
      fullrange=FALSE, 
      level=0.995
    ) + 
    labs (
      title= "",
      y="",
      x="PMA"
    ) +
    default_theming()
  return(plot)
} 


generator_lineareg_plot_grid_autotest_vs_others <- function(dataset){
  
  # Autotest x Oxford
  pOxford = generate_scattering_autotest_oxford(dataset)
  
  # Autotest x Womac
  pWomac = generate_scattering_autotest_womac(dataset)
  
  # Autotest x Harris
  pHarris = generate_scattering_autotest_harris(dataset)
  
  # Autotest x PMA 
  pPMA =  generate_scattering_autotest_pma(dataset)
  
  title <- ggdraw() +
    draw_label(
      "Corrélation des résultats de l'Autotest aux autres scores",
      x = 0,
      hjust = 0,
      vjust=0.5
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  
  pgrid = plot_grid(pOxford, pWomac, pHarris, pPMA, labels=c(NA, NA, NA, NA),ncol = 2, nrow = 2)
  
  plot = plot_grid(
    title, pgrid,
    labels=c(NA, NA),
    ncol=1, 
    rel_heights = c(0.1, 1)
  )
  return(plot)
}


#graph with linear regression lines for each score
generate_4_regression_lines_same_graph <- function(dataset){
  # Display 4 linear regression lines on 1 plot
  plot = dataset %>%
    ggplot( aes(x=Autotest, y=Oxford)) +
    geom_smooth(
      color=color_boxplot_scores("Oxford"),
      fill=color_boxplot_scores("Oxford"),
      aes(x=Autotest, y=Oxford),
      method="lm", 
      se=FALSE, 
      fullrange=FALSE, 
      level=0.95
    ) +   
    geom_smooth(
      color=color_boxplot_scores("Womac"),
      fill=color_boxplot_scores("Womac"),
      aes(x=Autotest, y=Womac),
      method="lm", 
      se=FALSE, 
      fullrange=FALSE, 
      level=0.95
    ) +
    geom_smooth(
      color=color_boxplot_scores("HarrisHS"),
      fill=color_boxplot_scores("HarrisHS"),
      aes(x=Autotest, y=(-HarrisHS)),
      method="lm", 
      se=FALSE, 
      fullrange=FALSE, 
      level=0.95
    ) +
    geom_smooth(
      color=color_boxplot_scores("PMA"),
      fill=color_boxplot_scores("PMA"),
      aes(x=Autotest, y=(-PMA)),
      method="lm", 
      se=FALSE, 
      fullrange=FALSE, 
      level=0.95
    )
  return(plot)
} 

generate_histogram_autotest_corr_with_other_scores <- function(dataset){
  
  #calculate correlations
  corOxford = spearman_autotest_oxford(mydata) # -> 0.8547031
  corWomac = spearman_autotest_womac(mydata) # -> 0.931137
  corHarris = spearman_autotest_harris(mydata) # -> -0.866056
  corPMA = spearman_autotest_pma(mydata) # -> 0.8928448
  
  #preparing data
  N <- 1e4
  testcors <- data.frame(
    TestName=character(),
    Rho=integer(),
    stringsAsFactors=FALSE
  ) 
  
  #storing results in dataframe
  testcors[1, ] <- list("Oxford",abs(corOxford))
  testcors[2, ] <- list("Womac", abs(corWomac))
  testcors[3, ] <- list("Harris", abs(corHarris))
  testcors[4, ] <- list("PMA", abs(corPMA))
  
  testcors$TestName <- factor(
    testcors$TestName, 
    levels=c(
      "Oxford",
      "Womac", 
      "Harris",
      "PMA" 
    )
  )
  
  #plotting dataframe as histogram
  plot = testcors %>%
    ggplot(aes(x=TestName, y=Rho, label=paste("ρ=",round(Rho, 3)))) +
    geom_bar(
      aes(fill = TestName),
      stat='identity',
      orientation="x",
      colour="white",
      alpha=0.8,
      width=0.7
    ) +
    geom_text(size = 4, position = position_stack(vjust = 0.9), color="white") +
    labs (
      title= "Corrélation de l'auto-test aux autres scores",
      x="Score",
      y="Corrélation de Spearman"
    ) +
    scale_fill_manual(
      values = c(
        color_boxplot_scores("Oxford"), 
        color_boxplot_scores("Womac"),
        color_boxplot_scores("HarrisHS"), 
        color_boxplot_scores("PMA")
      )
    )+ #custom values
    default_theming(
      y_title_angle=90,
      legend_position = "none"
    )
  
    return(plot)
}

# calcul de la correlation de spearman entre Autotest (x) et Oxford (y)
spearman_autotest_oxford <- function(dataset){
  # 
  corOxford = cor(x=dataset$Autotest, y=dataset$Oxford, method="spearman") # -> 0.8547031
  return(corOxford)
}

# calcul de la correlation de spearman entre Autotest (x) et Oxford (y)
spearman_autotest_womac <- function(dataset){
  corWomac= cor(x=dataset$Autotest, y=dataset$Womac, method="spearman") # -> 0.8547031
  return(corWomac)
}

# calcul de la correlation de spearman entre Autotest (x) et Oxford (y)
spearman_autotest_harris <- function(dataset){
  corHarris = cor(x=dataset$Autotest, y=dataset$HarrisHS, method="spearman") # -> 0.8547031
  return(corHarris)
}

# calcul de la correlation de spearman entre Autotest (x) et Oxford (y)
spearman_autotest_pma <- function(dataset){
  corPMA = cor(x=dataset$Autotest, y=dataset$PMA, method="spearman") # -> 0.8547031
  return(corPMA)
}

