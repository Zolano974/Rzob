source("utils/theming_plots.R")
source("utils/theming_colors.R")
#
# Generate stacked histogram to display the repartition of fracture types according to trauma
#
generate_plot_patients_repartition_by_traumatisme <- function(dataset) {
  
  #defining order for "Traumatisme"  (/!\ if orientation=x order needs to be reversed)
  dataset$Traumatisme <- factor(
    dataset$Traumatisme, 
    levels=c(
      "AVP",
      "Chute", 
      "Sport",
      "Agricole" 
    )
  )
  
  plot = dataset %>%
    ggplot(      aes(x=Traumatisme)
    ) +
    geom_bar(
      # position=
      stat='count', 
      orientation="x", 
      alpha=0.9,
      colour="white",
      fill=color_generic_stats(),
      width=0.7
    ) +
    # scale_y_continuous(labels=scales::percent) +
    labs (
      title= "Répartition des patients en fonction du type de trauma",
      y="Nombre de cas",
      x="Type de traumatisme"
    ) +
    default_theming(
      y_title_angle = 90,
      title_size = 14,
      legend_title_size = 12
    )
  
  return(plot)
  
}

generate_plot_patients_repartition_by_fracture <- function(dataset){

  dataset$FractureX <- as.character(dataset$Fracture)
  dataset$FractureX[dataset$Fracture == "paroi postérieure"] <- "PP"
  dataset$FractureX[dataset$Fracture == "transversale et paroi postérieure"] <- "TR-PP"
  dataset$FractureX[dataset$Fracture == "transversale"] <- "TR"
  dataset$FractureX[dataset$Fracture == "bi colonne"] <- "BC"
  dataset$FractureX[dataset$Fracture == "T"] <- "T"
  dataset$FractureX[dataset$Fracture == "colonne antérieure"] <- "CA"
  
  dataset$FractureX <- factor(
    dataset$FractureX, 
    levels=c(
      "PP",
      "TR-PP",
      "TR",
      "BC",
      "T",
      "CA"
    )
  )
  
  plot = dataset %>%
    ggplot(aes(x=FractureX)) +
    geom_bar(
      stat='count', 
      orientation="x", 
      alpha=0.9,
      colour="white",
      fill=color_generic_stats(),
      width=0.7
    ) +
    labs (
      title= "Répartition des patients en fonction du type de fracture",
      y="Nombre de cas",
      x="Type de Fracture"
    ) +
    default_theming(
      y_title_angle = 90,
      title_size = 14,
      legend_title_size = 12,
      x_text_angle = 45,
      x_text_size = 10,
      x_text_vjust = 0.5
    )
  
  return(plot)
}

generate_plot_patients_repartition_by_voie_abord <- function(dataset) {
  

  #defining order for "Chirurgien"  (/!\ if orientation=x order needs to be reversed)
  dataset$Abord <- factor(
    dataset$Abord, 
    levels=c(
      "Ilio",
      "Kocher", 
      "Double"
    )
  )
  
  plot = dataset %>%
    ggplot(      aes(x=Abord)
    ) +
    geom_bar(
      # position=
      stat='count', 
      orientation="x", 
      alpha=0.9,
      colour="white",
      fill=color_generic_stats(),
      width=0.7
    ) +
    # scale_y_continuous(labels=scales::percent) +
    labs (
      title= "Répartition des patients en fonction de la voie d'abord",
      y="Nombre de cas",
      x="Abord"
    ) +
    default_theming(
      y_title_angle = 90,
      title_size = 14,
      legend_title_size = 12
    )
  
  return(plot)
  
}

generate_plot_patients_repartition_by_xp_chir <- function(dataset) {
  
  #defining order for "Chirurgien"  (/!\ if orientation=x order needs to be reversed)
  dataset$Chirurgien <- factor(
    dataset$Chirurgien, 
    levels=c(
      "Junior", 
      "Senior",
      "Expérimenté"
    )
  )
  
  plot = dataset %>%
    ggplot(      aes(x=Chirurgien)
    ) +
    geom_bar(
      # position=
      stat='count', 
      orientation="x", 
      alpha=0.9,
      colour="white",
      fill=color_generic_stats(),
      width=0.7
    ) +
    # scale_y_continuous(labels=scales::percent) +
    labs (
      title= "Répartition des patients en fonction de l'expérience du Chirurgien",
      y="Nombre de cas",
      x="Chirurgien"
    ) +
    default_theming(
      y_title_angle = 90,
      title_size = 14,
      legend_title_size = 12
    )
  
  return(plot)
  
}


generate_plot_patients_repartition_by_matta <- function(dataset) {
  
  dataset$MattaFull <- as.character(dataset$Matta)
  dataset$MattaFull[dataset$Matta == "1"] <- "Anatomique"
  dataset$MattaFull[dataset$Matta == "2"] <- "Satisfaisant"
  dataset$MattaFull[dataset$Matta == "3"] <- "Non Satisfaisant"
  
  #defining order for "Chirurgien"  (/!\ if orientation=x order needs to be reversed)
  dataset$MattaFull <- factor(
    dataset$MattaFull, 
    levels=c(
      "Anatomique", 
      "Satisfaisant",
      "Non Satisfaisant"
    )
  )
  
  plot = dataset %>%
    ggplot(      aes(x=MattaFull)
    ) +
    geom_bar(
      # position=
      stat='count', 
      orientation="x", 
      alpha=0.9,
      colour="white",
      fill=color_generic_stats(),
      width=0.7
    ) +
    # scale_y_continuous(labels=scales::percent) +
    labs (
      title= "Répartition des patients en fonction du score de Matta",
      y="Nombre de cas",
      x="Matta"
    ) +
    default_theming(
      y_title_angle = 90,
      title_size = 14,
      legend_title_size = 12
    )
  
  return(plot)
  
}

