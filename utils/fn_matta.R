#matta functions

display_matta <- function(matta){
  print("matta is : ")
  print(matta)
  #1 -> anatomique, 2 -> satisfaisant, 3 -> non satisfaisant
  if(matta == 1){
    return("Anatomique")
  }
  else if(matta == 2){
    return("satisfaisant")
  }else{
    return("non satisfaisant")
  }
}

generate_histogram_mata_expchir <- function(dataset){
  # replace Matta numeric values by display values
  dataset$MattaFull <- as.character(dataset$Matta)
  dataset$MattaFull[dataset$Matta == "1"] <- "Anatomique"
  dataset$MattaFull[dataset$Matta == "2"] <- "Satisfaisant"
  dataset$MattaFull[dataset$Matta == "3"] <- "Non Satisfaisant"
  
  #order by chirurgien custom
  dataset$Chirurgien <- factor(
    dataset$Chirurgien, 
    levels=c(
      "Junior", 
      "Senior",
      "Expérimenté"
    )
  )
  
  #order by matta custom
  dataset$MattaFull <- factor(
    dataset$MattaFull, 
    levels=c(
      "Anatomique", 
      "Satisfaisant",
      "Non Satisfaisant"
    )
  )
  
  plot = dataset %>%
    ggplot(aes(x=Chirurgien, fill=MattaFull)) +
    geom_bar(
      stat='count',
      position="fill",
      orientation="x",
      colour="white"
    ) +
    labs (
      title= "Score de Matta en fonction de l'expérience du chirurgien",
      x="Expérience du chirurgien",
      y="%"
    ) +
    theme(
      legend.position = "right",
      axis.title.y = element_text(colour= "red")
    )
  
  return(plot)
}

generate_histogram_mata_fracture <- function(dataset){
  # replace Matta numeric values by display values
  dataset$MattaFull <- as.character(dataset$Matta)
  dataset$MattaFull[dataset$Matta == "1"] <- "Anatomique"
  dataset$MattaFull[dataset$Matta == "2"] <- "Satisfaisant"
  dataset$MattaFull[dataset$Matta == "3"] <- "Non Satisfaisant"
  
  #order by matta custom
  dataset$MattaFull <- factor(
    dataset$MattaFull, 
    levels=c(
      "Anatomique", 
      "Satisfaisant",
      "Non Satisfaisant"
    )
  )
  
  plot = dataset %>%
    ggplot(aes(x=factor(
      Fracture, 
      #here we define the order of the stacked values
      levels=c(
        "T",
        "colonne antérieure",
        "transversale",
        "paroi postérieure", 
        "transversale et paroi postérieure",
        "bi colonne"
      )
    ), fill=MattaFull)) +
    geom_bar(
      stat='count',
      position="fill",
      orientation="x",
      colour="white"
    ) +
    labs (
      title= "Répartition du score de Matta en fonction du type de fracture",
      x="Type de Fracture",
      y="%"
    ) +
    theme(
      legend.position = "right",
      axis.title.y = element_text(colour= "black"),
      axis.text.x =  element_text(angle = 50,vjust=0.5),
    )
  
  return(plot)
}

generate_histogram_fracture_expchir <- function(dataset){

  
  #order by chirurgien custom
  dataset$Chirurgien <- factor(
    dataset$Chirurgien, 
    levels=c(
      "Junior", 
      "Senior",
      "Expérimenté"
    )
  )

  
  plot = dataset %>%
    ggplot(aes(x=Chirurgien, fill=factor(
      Fracture, 
      #here we define the order of the stacked values
      levels=c(
        "T",
        "colonne antérieure",
        "transversale",
        "paroi postérieure", 
        "transversale et paroi postérieure",
        "bi colonne"
      )
    )
    )) +
    geom_bar(
      stat='count',
      position="fill",
      orientation="x",
      colour="white"
    ) +
    labs (
      title= "Type de Fracture en fonction de l'expérience du chirurgien",
      x="Expérience du chirurgien",
      y="%"
    ) +
    theme(
      legend.position = "right",
      axis.title.y = element_text(colour= "red")
    )
  
  return(plot)
}