calculate_sf36_means <- function(dataset){
  
  mean_sf36_fonctionphysique = round(mean(dataset$fonctionphysique), 2)
  mean_sf36_limitationphysique = round(mean(dataset$limitationphysique), 2)
  mean_sf36_limitationemotionnelle = round(mean(dataset$limitationemotionnelle), 2)
  mean_sf36_energiefatigue = round(mean(dataset$energiefatigue), 2)
  mean_sf36_bienetre = round(mean(dataset$bienetre), 2)
  mean_sf36_fonctionsociale = round(mean(dataset$fonctionsociale), 2)
  mean_sf36_douleur = round(mean(dataset$douleur), 2)
  mean_sf36_santegenerale = round(mean(dataset$santegenerale), 2)
  mean_sf36_modificationsantee = round(mean(dataset$modificationsante), 2)
  
  sf36_means <- hash()
                .set(sf36_means,
                  fonctionphysique=mean_sf36_fonctionphysique,
                  limitationphysique=mean_sf36_limitationphysique,
                  limitationemotionnelle=mean_sf36_limitationemotionnelle,
                  energiefatigue=mean_sf36_energiefatigue,
                  bienetre=mean_sf36_bienetre,
                  fonctionsociale=mean_sf36_fonctionsociale,
                  douleur=mean_sf36_douleur,
                  santegenerale=mean_sf36_santegenerale,
                  modificationsante=mean_sf36_modificationsantee
                )
    return(sf36_means)
}

init_circular_plotdata <- function(dataset){
  
  #init empty dataframe
  N <- 1e4
  plotdata <- data.frame(
    id=integer(),
    category=character(),
    score=integer(),
    stringsAsFactors=FALSE
  )
  
  #fetch calculated means
  sf36_means = calculate_sf36_means(dataset)
  
  #fill plotdata with values
  plotdata[1, ] <- list(1, "fonctionphysique", sf36_means[["fonctionphysique"]])
  plotdata[2, ] <- list(2,"limitationphysique", sf36_means[["limitationphysique"]])
  plotdata[3, ] <- list(3,"limitationemotionnelle", sf36_means[["limitationemotionnelle"]])
  plotdata[4, ] <- list(4, "energiefatigue", sf36_means[["energiefatigue"]])
  plotdata[5, ] <- list(5,"bienetre", sf36_means[["bienetre"]])
  plotdata[6, ] <- list(6,"fonctionsociale", sf36_means[["fonctionsociale"]])
  plotdata[7, ] <- list(7,"douleur", sf36_means[["douleur"]])
  plotdata[8, ] <- list(8,"santegenerale", sf36_means[["santegenerale"]])
  plotdata[9, ] <- list(9, "modificationsante", sf36_means[["modificationsante"]])
  
  #order values
  plotdata$category <- factor(
    plotdata$category,
    levels=c(
      "fonctionphysique",
      "limitationphysique",
      "limitationemotionnelle",
      "bienetre",
      "energiefatigue",
      "fonctionsociale",
      "douleur",
      "santegenerale",
      "modificationsante"
    )
  )
  
  return(plotdata)
}

init_circular_labelsdata <- function(plotdata){
  
  labels_data <- plotdata
  
  # calculate the ANGLE of the labels
  number_of_bar <- nrow(labels_data)
  angle <-  90 - 360 * (labels_data$id - 0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90 (in fact 120, because of the few number of values)
  labels_data$hjust<-ifelse( angle< -90, -0.5, 1.5)
  
  # flip angle BY to make them readable
  labels_data$angle<-ifelse(angle < -90, angle+180, angle)
  # labels_data$angle<-ifelse(angle < -90, angle, angle)
  
  return(labels_data)
}

generate_SF36_circularo_barplot <- function(dataset){
  
  #init plot data
  plotdata = init_circular_plotdata(dataset)
  
  #init labels data
  labels = init_circular_labelsdata(plotdata)
  
  # View(label_data)
  
  plot = plotdata %>%
    ggplot(aes(x=as.factor(id), y=score,fill=category)) +
    geom_bar(stat="identity") +
    ylim(-35,120) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
    ) +
    coord_polar(start = 0) +
    scale_fill_brewer(palette="Paired") +
    geom_text(
      data=labels, 
      aes(
        x=id, 
        y=score, 
        label=score,
        hjust=hjust
      ), 
      color="black", 
      fontface="bold",
      alpha=0.6, 
      size=4, 
      angle= labels$angle, 
      inherit.aes = FALSE 
    )
  
  return(plot)
  
}