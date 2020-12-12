#matta functions

source("utils/fn_fractures.R")
source("utils/theming_plots.R")
source("utils/theming_colors.R")

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
  
  # label=round(Rho, 3)
  
  plot = dataset %>%
    ggplot(aes(x=Chirurgien, fill=MattaFull)) +
    geom_bar(
      stat='count',
      position="fill",
      orientation="x",
      colour="white",
      width=0.8
    ) +
    scale_y_continuous(labels=scales::percent) +
    labs (
      title= "",
      x="Expérience du chirurgien",
      fill="Matta",
      y=""
    ) +
    scale_fill_brewer(palette=palette_matta()) +
    default_theming(
      y_title_vjust = 0.5,
      title_size = 16
    )
  return(plot)
}

generate_histogram_mata_fracture <- function(dataset){
  # replace Matta numeric values by display values
  dataset$MattaFull <- as.character(dataset$Matta)
  dataset$MattaFull[dataset$Matta == "1"] <- "Anatomique"
  dataset$MattaFull[dataset$Matta == "2"] <- "Satisfaisant"
  dataset$MattaFull[dataset$Matta == "3"] <- "Non Satisfaisant"
  
  #replace Fracture by shortcut
  dataset$FractureX <- as.character(dataset$Fracture)
  dataset$FractureX[dataset$Fracture == "paroi postérieure"] <- "PP"
  dataset$FractureX[dataset$Fracture == "transversale et paroi postérieure"] <- "TR-PP"
  dataset$FractureX[dataset$Fracture == "transversale"] <- "TR"
  dataset$FractureX[dataset$Fracture == "bi colonne"] <- "BC"
  dataset$FractureX[dataset$Fracture == "T"] <- "T"
  dataset$FractureX[dataset$Fracture == "colonne antérieure"] <- "CA"
  
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
      FractureX, 
      #here we define the order of the stacked values
      levels=factor_fracture_x_levels(),
    ), fill=MattaFull)) +
    geom_bar(
      stat='count',
      position="fill",
      orientation="x",
      colour="white",
      width=0.8
    ) +
    scale_y_continuous(labels=scales::percent) +
    labs (
      title= "",
      x="Type de Fracture",
      fill="Matta",
      y=""
    ) + 
    scale_fill_brewer(palette=palette_matta()) +
    # scale_fill_manual(values = c("#04c70a", "#fcff3d", "#FF0000")) #custom values
    default_theming(
        x_text_size = 12,
        x_text_angle=50,
        x_text_vjust = 0.5,
        y_text_size=10,
        y_title_vjust = 0.5,
        title_size = 16
    )
  
  return(plot)
}


