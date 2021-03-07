source("utils/theming_plots.R")
source("utils/theming_colors.R")



percentage_harris_category <- function(dataset, harris_category){
  
  total = nrow(dataset)
  
  t_harris_category <- quote(harris_category)
  occurrences = nrow(dataset[HarrisCategory == eval(t_harris_category)])
  
  if(occurrences == 0){
    return(0)
  }
  return(round((occurrences / total), 3) *100 )
  
}


percentage_pma_category <- function(dataset, pma_category){
  
  total = nrow(dataset)
  
  t_pma_category <- quote(pma_category)
  occurrences = nrow(dataset[PMACategory == eval(t_pma_category)])
  
  if(occurrences == 0){
    return(0)
  }
  return(round((occurrences / total), 3) *100 )
  
}

#calculate the % of the given trauma over the given dataset
percentage_trauma <- function(dataset, trauma){
  
  # View(dataset)
  
  total = nrow(dataset)
  
  t_trauma <- quote(trauma)
  occurrences = nrow(dataset[Traumatisme == eval(t_trauma)])
  
  print(paste("trauma :", t_trauma))
  print(paste("occurrences :", occurrences))
  print(paste("total :", total))
  
  if(occurrences == 0){
    return(0)
  }
  return(round((occurrences / total), 3) *100 )

}

percentage_fracture <- function(dataset, fracture){
  
  total = nrow(dataset)
  
  t_fracture <- quote(fracture)
  occurrences = nrow(dataset[Fracture == eval(t_fracture)])
  
  if(occurrences == 0){
    return(0)
  }
  return(round((occurrences / total), 3) *100 )
  
}

percentage_abord <- function(dataset, abord){
  
  total = nrow(dataset)
  
  t_abord <- quote(abord)
  occurrences = nrow(dataset[Abord == eval(t_abord)])
  
  if(occurrences == 0){
    return(0)
  }
  return(round((occurrences / total), 3) *100 )
}

percentage_xpchir <- function(dataset, xpchir){
  
  total = nrow(dataset)
  
  t_xpchir <- quote(xpchir)
  occurrences = nrow(dataset[Chirurgien == eval(t_xpchir)])
  
  if(occurrences == 0){
    return(0)
  }
  return(round((occurrences / total), 3) *100 )
}

percentage_matta <- function(dataset, matta){
  
  total = nrow(dataset)
  
  t_matta <- quote(matta)
  occurrences = nrow(dataset[Matta == eval(t_matta)])
  
  if(occurrences == 0){
    return(0)
  }
  return(round((occurrences / total), 3) *100 )
}

#
# Generate stacked histogram to display the repartition of fracture types according to trauma
#
generate_plot_patients_repartition_by_traumatisme <- function(dataset) {
  
  N <- 1e4
  plotdataset <- data.frame(
    Traumatisme=character(),
    Percentage=integer(),
    stringsAsFactors=FALSE
  )
  
  # View(dataset)
  
  plotdataset[1, ] <- list("AVP", percentage_trauma(dataset, "AVP"))
  plotdataset[2, ] <- list("Chute", (percentage_trauma(dataset, "Chute") - 0.1)) #round for total=100
  plotdataset[3, ] <- list("Sport", percentage_trauma(dataset, "Sport"))
  plotdataset[4, ] <- list("Agricole", percentage_trauma(dataset, "Agricole"))
  
  plotdataset$Traumatisme <- factor(
    plotdataset$Traumatisme,
    levels=c(
      "AVP",
      "Chute",
      "Sport",
      "Agricole"
    )
  )

  plot = plotdataset %>%
    ggplot( aes(x = Traumatisme, y = Percentage)) +
    # scale_y_continuous(position="None") +
    geom_col(
      alpha=0.6,
      fill=color_generic_stats(),
      width=0.7
    ) +
    geom_text(aes(label = paste(Percentage, "%")), vjust = -0.3, colour = "black", size=6) +
    labs (
      title= "",
      y="Nombre de cas",
      x="Type de traumatisme"
    ) +
    default_theming(
      y_title_angle = 90,
      x_text_size = 15, 
      title_size = 14,
      legend_title_size = 12,
      y_text_size = 0,
      y_title_size = 0
    )
  return(plot)
  
}

generate_plot_patients_repartition_by_fracture <- function(dataset){
  
  N <- 1e4
  plotdataset <- data.frame(
    Fracture=character(),
    Percentage=integer(),
    stringsAsFactors=FALSE
  )
  
  plotdataset[1, ] <- list("PP", (percentage_fracture(dataset, "paroi postérieure")-0.1)) #round for total = 100
  plotdataset[2, ] <- list("TR-PP", percentage_fracture(dataset, "transversale et paroi postérieure"))
  plotdataset[3, ] <- list("TR", percentage_fracture(dataset, "transversale"))
  plotdataset[4, ] <- list("BC", percentage_fracture(dataset, "bi colonne"))
  plotdataset[5, ] <- list("T", percentage_fracture(dataset, "T"))
  plotdataset[6, ] <- list("CA", percentage_fracture(dataset, "colonne antérieure"))
  
  plotdataset$Fracture <- factor(
    plotdataset$Fracture,
    levels=c(
      "PP",
      "TR-PP",
      "TR",
      "BC",
      "T",
      "CA"
    )
  )
  
  plot = plotdataset %>%
    ggplot( aes(x = Fracture, y = Percentage)) +
    geom_col(
      alpha=0.6,
      fill=color_generic_stats(),
      width=0.7
    ) +
    geom_text(aes(label = paste(Percentage, "%")), vjust = -0.3, colour = "black",size=6) +
    labs (
      title= "",
      y="Nombre de cas",
      x="Type de fracture"
    ) +
    default_theming(
      y_title_angle = 90,
      x_text_size=15,
      title_size = 14,
      legend_title_size = 12,
      y_text_size = 0,
      y_title_size = 0
    )
  
  return(plot)
}

generate_plot_patients_repartition_by_voie_abord <- function(dataset) {
  
  
  N <- 1e4
  plotdataset <- data.frame(
    Abord=character(),
    Percentage=integer(),
    stringsAsFactors=FALSE
  )
  
  plotdataset[1, ] <- list("Ilio-inguinal", percentage_abord(dataset, "Ilio"))
  plotdataset[2, ] <- list("Kocher-Langenbeck", percentage_abord(dataset, "Kocher"))
  plotdataset[3, ] <- list("Double Abord", percentage_abord(dataset, "Double"))
  
  #defining order for "Chirurgien"  (/!\ if orientation=x order needs to be reversed)
  plotdataset$Abord <- factor(
    plotdataset$Abord, 
    levels=c(
      "Ilio-inguinal",
      "Kocher-Langenbeck", 
      "Double Abord"
    )
  )
  
  plot = plotdataset %>%
    ggplot( aes(x = Abord, y = Percentage)) +
    geom_col(
      alpha=0.6,
      fill=color_generic_stats(),
      width=0.7
    ) +
    geom_text(aes(label = paste(Percentage, "%")), vjust = -0.3, colour = "black", size=6) +
    labs (
      title= "",
      y="Nombre de cas",
      x="Voie d'Abord"
    ) +
    default_theming(
      y_title_angle = 90,
      x_text_size=15,
      title_size = 14,
      legend_title_size = 12,
      y_text_size = 0,
      y_title_size = 0
    )
  
  return(plot)
  
}

generate_plot_patients_repartition_by_xp_chir <- function(dataset) {
  N <- 1e4
  plotdataset <- data.frame(
    Chirurgien=character(),
    Percentage=integer(),
    stringsAsFactors=FALSE
  )
  
  plotdataset[1, ] <- list("Junior", percentage_xpchir(dataset, "Junior"))
  plotdataset[2, ] <- list("Senior", percentage_xpchir(dataset, "Senior"))
  plotdataset[3, ] <- list("Expérimenté", percentage_xpchir(dataset, "Expérimenté"))
  
  plotdataset$Chirurgien <- factor(
    plotdataset$Chirurgien, 
    levels=c(
      "Junior", 
      "Senior",
      "Expérimenté"
    )
  )
  
  plot = plotdataset %>%
    ggplot( aes(x = Chirurgien, y = Percentage)) +
    geom_col(
      alpha=0.6,
      fill=color_generic_stats(),
      width=0.7
    ) +
    geom_text(aes(label = paste(Percentage, "%")), vjust = -0.3, colour = "black", size=6) +
    labs (
      title= "",
      y="Nombre de cas",
      x="Expérience du Chirurgien"
    ) +
    default_theming(
      y_title_angle = 90,
      x_text_size = 17,
      title_size = 14,
      legend_title_size = 12,
      y_text_size = 0,
      y_title_size = 0
    )
  return(plot)
  
}


generate_plot_patients_repartition_by_matta <- function(dataset) {
  N <- 1e4
  plotdataset <- data.frame(
    Matta=character(),
    Percentage=integer(),
    stringsAsFactors=FALSE
  )
  
  plotdataset[1, ] <- list("Anatomique", percentage_matta(dataset, "1"))
  plotdataset[2, ] <- list("Satisfaisant", percentage_matta(dataset, "2"))
  plotdataset[3, ] <- list("Non Satisfaisant", percentage_matta(dataset, "3"))
  
  #defining order for "Chirurgien"  (/!\ if orientation=x order needs to be reversed)
  plotdataset$Matta <- factor(
    plotdataset$Matta, 
    levels=c(
      "Anatomique", 
      "Satisfaisant",
      "Non Satisfaisant"
    )
  )
  
  plot=plotdataset %>%
    ggplot( aes(x = Matta, y = Percentage)) +
    geom_col(
      alpha=0.6,
      fill=color_generic_stats(),
      width=0.7
    ) +
    geom_text(aes(label = paste(Percentage, "%")), vjust = -0.3, colour = "black", size=6) +
    labs (
      title= "",
      y="Nombre de cas",
      x="Matta"
    ) +
    default_theming(
      y_title_angle = 90,
      x_text_size=17,
      title_size = 14,
      legend_title_size = 12,
      y_text_size = 0,
      y_title_size = 0
    )
  return(plot)
  
}

