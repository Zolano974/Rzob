#Functions related to Fracture : 
# Fracture repartition by age, traumatisme


#
# Generate stacked histogram to display the repartition of fracture types according to trauma
#
generate_plot_fracture_by_traumatisme <- function(dataset) {
  
  #defining order for "Traumatisme"  (/!\ if orientation=x order needs to be reversed)
  dataset$Traumatisme <- factor(
    dataset$Traumatisme, 
    levels=c(
      "Agricole", 
      "Sport",
      "Chute", 
      "AVP"
    )
  )

  plot = dataset %>%
    ggplot(
      aes(y=Traumatisme, fill=Fracture, order=Fracture)) +
    geom_bar(
      stat='count', 
      orientation="y", 
      alpha=0.9,
      colour="white"
    ) +
    labs (
      title= "Répartition des fractures en fonction du type de trauma",
      x="Nombre de Fractures",
      y="Type de traumatisme"
    ) +
    default_theming()
  
  return(plot)

}

#
# Generate stacked histogram to display the repartition of fracture types according to age
#
generate_plot_fracture_by_age <- function(dataset){
  
  plot = dataset %>%
    ggplot(aes(x=Age, fill=factor(
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
      )
    ) +
    geom_histogram(
      binwidth=15,
      orientation="x",
      colour="white"
    ) +
    labs (
      title= "Répartition des fractures en fonction de l'âge",
      x="Classe d'âge",
      y="Nombre de fractures"
    ) +
    default_theming()
  
  return(plot)
}





