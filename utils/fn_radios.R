
source("utils/theming_plots.R")

generate_plot_arthrose_by_matta <- function(dataset){
  
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
  
  #aggregate 1-2 and 3-4
  dataset$ArthroseFull <- as.character(dataset$Arthrose)
  dataset$ArthroseFull[dataset$Arthrose == "1" | dataset$Arthrose == "2"] <- "1-2"
  dataset$ArthroseFull[dataset$Arthrose == "3" | dataset$Arthrose == "4"] <- "3-4"
  dataset$ArthroseFull[dataset$Arthrose == "0"] <- "0"
  
  dataset$Arthrose <- factor(
    dataset$ArthroseFull,
    levels=c(
      "3-4",
      "1-2",
      "0"
    )
  )
  
  plot = ggplot(dataset, aes(
        x = factor(MattaFull), 
        fill = factor(Arthrose), 
        y = (..count..)/sum(..count..)
      )
    ) +
    geom_bar(
      position="stack",
      colour="white",
      alpha=0.8,
      width=0.8
    ) +
    # scale_y_continuous(labels=scales::percent) +
    labs (
      title= "",
      x="Matta",
      fill="Arthrose",
      y=""
    ) + 
    stat_count(
      geom = "text",
      position="stack",
      # aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
      aes(label = ..count..),
      size=2.5,
      vjust = 1.5
    ) +
    scale_fill_brewer(palette=palette_radios()) +
    default_theming(
      x_text_size = 12,
      x_text_vjust = 0.5,
      y_text_size=0,
      y_title_vjust = 0.5,
      title_size = 16
    )
  
  return(plot)
}

generate_plot_arthrose_by_fracture <- function(dataset){
  
  #replace Fracture by shortcut
  dataset$FractureX <- as.character(dataset$Fracture)
  dataset$FractureX[dataset$Fracture == "paroi postérieure"] <- "PP"
  dataset$FractureX[dataset$Fracture == "transversale et paroi postérieure"] <- "TR-PP"
  dataset$FractureX[dataset$Fracture == "transversale"] <- "TR"
  dataset$FractureX[dataset$Fracture == "bi colonne"] <- "BC"
  dataset$FractureX[dataset$Fracture == "T"] <- "T"
  dataset$FractureX[dataset$Fracture == "colonne antérieure"] <- "CA"
  
  #aggregate 1-2 and 3-4
  dataset$ArthroseFull <- as.character(dataset$Arthrose)
  dataset$ArthroseFull[dataset$Arthrose == "1" | dataset$Arthrose == "2"] <- "1-2"
  dataset$ArthroseFull[dataset$Arthrose == "3" | dataset$Arthrose == "4"] <- "3-4"
  dataset$ArthroseFull[dataset$Arthrose == "0"] <- "0"

  
  dataset$ArthroseFull <- factor(
    dataset$ArthroseFull,
    levels=c(
      "3-4",
      "1-2",
      "0"
    )
  )
  
  plot = dataset %>%
    ggplot(aes(x=factor(
                FractureX, 
                levels=factor_fracture_x_levels(),
              ), 
              fill=ArthroseFull,
              y = (..count..)/sum(..count..)
          )
    ) +
    geom_bar(
      position="stack",
      colour="white",
      alpha=0.8,
      width=0.8
    ) +
    # scale_y_continuous(labels=scales::percent) +
    labs (
      title= "",
      x="Fracture",
      fill="Arthrose",
      y=""
    ) + 
    stat_count(
      geom = "text",
      position="stack",
      # aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
      aes(label = ..count..),
      size=2.5,
      vjust = 2
    ) +    
    scale_fill_brewer(palette=palette_radios()) +
    default_theming(
      x_text_size = 12,
      x_text_angle=50,
      x_text_vjust = 0.5,
      y_text_size=0,
      y_title_vjust = 0.5,
      title_size = 16
    )
  
  return(plot)
}

generate_plot_ossification_by_matta <- function(dataset){
  
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
  
  dataset$Ossifications <- factor(
    dataset$Ossifications,
    levels=c(
      "4",
      "3",
      "2",
      "1",
      "0"
    )
  )
  
  plot = dataset %>%
    ggplot(aes(
      x=factor(MattaFull), 
      fill=Ossifications),
      y = (..count..)/sum(..count..)
    ) +
    geom_bar(
      position="stack",
      orientation="x",
      colour="white",
      alpha=0.8,
      width=0.8
    ) +
    # scale_y_continuous(labels=scales::percent) +
    labs (
      title= "",
      x="Matta",
      fill="Ossification",
      y=""
    ) + 
    stat_count(
      geom = "text",
      position="stack",
      # aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
      aes(label = ..count..),
      size=2.5,
      vjust = 2
    ) +      
    scale_fill_brewer(palette=palette_radios()) +
    default_theming(
      x_text_size = 12,
      x_text_vjust = 0.5,
      y_text_size=0,
      y_title_vjust = 0.5,
      title_size = 16
    )
  
  return(plot)
}

generate_plot_ossification_by_fracture <- function(dataset){
  
  #replace Fracture by shortcut
  dataset$FractureX <- as.character(dataset$Fracture)
  dataset$FractureX[dataset$Fracture == "paroi postérieure"] <- "PP"
  dataset$FractureX[dataset$Fracture == "transversale et paroi postérieure"] <- "TR-PP"
  dataset$FractureX[dataset$Fracture == "transversale"] <- "TR"
  dataset$FractureX[dataset$Fracture == "bi colonne"] <- "BC"
  dataset$FractureX[dataset$Fracture == "T"] <- "T"
  dataset$FractureX[dataset$Fracture == "colonne antérieure"] <- "CA"
  
  
  dataset$Ossifications <- factor(
    dataset$Ossifications,
    levels=c(
      "4",
      "3",
      "2",
      "1",
      "0"
    )
  )
  
  plot = dataset %>%
    ggplot(aes(x=factor(
                  FractureX, 
                  levels=factor_fracture_x_levels(),
                ), 
                fill=Ossifications,
                y = (..count..)/sum(..count..)
              )
    ) +
    geom_bar(
      position="stack",
      orientation="x",
      colour="white",
      alpha=0.8,
      width=0.8
    ) +
    # scale_y_continuous(labels=scales::percent) +
    labs (
      title= "",
      x="Fracture",
      fill="Ossification",
      y=""
    ) + 
    stat_count(
      geom = "text",
      position="stack",
      # aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
      aes(label = ..count..),
      size=2.5,
      vjust = 2
    ) +        
    scale_fill_brewer(palette=palette_radios()) +
    default_theming(
      x_text_size = 12,
      x_text_angle=50,
      x_text_vjust = 0.5,
      y_text_size=0,
      y_title_vjust = 0.5,
      title_size = 16
    )
  
  return(plot)
}